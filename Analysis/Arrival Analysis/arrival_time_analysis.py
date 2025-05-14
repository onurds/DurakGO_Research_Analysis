import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime
import os
from pathlib import Path
import statsmodels.api as sm

# Set up paths
base_dir = Path(__file__).parent.parent
schedule_data_dir = base_dir / "Analysis" / "Arrival_Data" / "schedule_data"
record_data_dir = base_dir / "Analysis" / "Arrival_Data" / "record_data"
survey_data_dir = base_dir / "Analysis" / "Survey_Data"
output_dir = base_dir / "Analysis" / "Output"

# Create output directory if it doesn't exist
os.makedirs(output_dir, exist_ok=True)

# Load schedule data
def load_schedule_data():
    lines = pd.read_csv(schedule_data_dir / "lines.csv")
    stations = pd.read_csv(schedule_data_dir / "stations.csv")
    directions = pd.read_csv(schedule_data_dir / "directions.csv")
    station_order = pd.read_csv(schedule_data_dir / "station_order.csv")
    times = pd.read_csv(schedule_data_dir / "times.csv")
    
    return {
        "lines": lines,
        "stations": stations,
        "directions": directions,
        "station_order": station_order,
        "times": times
    }

# Load record data
def load_record_data():
    record_arrival = pd.read_csv(record_data_dir / "record_arrival.csv")
    return record_arrival

# Load survey data
def load_survey_data():
    survey_data = pd.read_csv(survey_data_dir / "final_data.csv")
    return survey_data

# Compare schedule data with actual data and calculate metrics
def analyze_arrival_accuracy(record_data, schedule_data):
    # For this analysis, the schedule times need to be merged with actual times
    
    # Prepare the schedule times data (full dataset)
    schedule_times_full = schedule_data["times"]
    
    # Get unique combinations of lineId, directionId, stationId, and dayType from record_data
    unique_keys = record_data[['lineId', 'directionId', 'stationId', 'dayType']].drop_duplicates()
    
    # Filter schedule_times to only include entries matching the record_data
    schedule_times = pd.merge(
        unique_keys,
        schedule_times_full, 
        on=['lineId', 'directionId', 'stationId', 'dayType'],
        how='inner'
    )
    
    print(f"Original schedule_times size: {len(schedule_times_full)} rows")
    print(f"Filtered schedule_times size: {len(schedule_times)} rows")
    print(f"Record data size: {len(record_data)} rows")
    
    # Create columns to convert time strings to minutes since midnight for comparison
    def time_to_minutes(time_str):
        time_obj = datetime.strptime(time_str, "%H:%M")
        return time_obj.hour * 60 + time_obj.minute
    
    # Add scheduled time to actual_times by matching lineId, directionId, stationId, and dayType
    # For each actual arrival time, find the closest scheduled time
    merged_data_list = []
    
    for _, actual_row in record_data.iterrows():
        # Find matching scheduled times
        matching_schedules = schedule_times[
            (schedule_times['lineId'] == actual_row['lineId']) &
            (schedule_times['directionId'] == actual_row['directionId']) &
            (schedule_times['stationId'] == actual_row['stationId']) &
            (schedule_times['dayType'] == actual_row['dayType'])
        ]
        
        if not matching_schedules.empty:
            # Convert actual time to minutes
            actual_minutes = time_to_minutes(actual_row['time'])
            
            # Create a copy to avoid SettingWithCopyWarning when modifying matching_schedules
            matching_schedules_copy = matching_schedules.copy()
            
            # Convert all matching scheduled times to minutes
            matching_schedules_copy.loc[:, 'scheduled_minutes'] = matching_schedules_copy['time'].apply(time_to_minutes)
            
            # Calculate absolute differences to find closest scheduled time
            matching_schedules_copy.loc[:, 'time_diff'] = abs(matching_schedules_copy['scheduled_minutes'] - actual_minutes)
            
            # Get the closest scheduled time
            closest_schedule = matching_schedules_copy.loc[matching_schedules_copy['time_diff'].idxmin()]
            
            # Create a combined row
            combined_row = actual_row.copy()
            combined_row['time_scheduled'] = closest_schedule['time']
            combined_row['scheduled_minutes'] = closest_schedule['scheduled_minutes']
            combined_row['actual_minutes'] = actual_minutes
            
            merged_data_list.append(combined_row)
    
    # Create merged dataframe
    merged_data = pd.DataFrame(merged_data_list)
    
    # Calculate absolute prediction error (APE)
    merged_data["ape"] = abs(merged_data["actual_minutes"] - merged_data["scheduled_minutes"])
    
    # Calculate directional bias (positive: predicted time later than actual, negative: earlier)
    merged_data["bias"] = merged_data["scheduled_minutes"] - merged_data["actual_minutes"]
    
    # Calculate accuracy as a percentage based on deviation
    # Using a formula where 0 min deviation = 100% accurate, 
    # and accuracy decreases linearly as deviation increases
    # At 5 minutes deviation, accuracy becomes 0%
    def calculate_accuracy_percentage(row):
        deviation = abs(row["scheduled_minutes"] - row["actual_minutes"])
        # Linear formula: max(0, 100 - 20*deviation)
        # 0 min deviation = 100%, 5 min deviation = 0%
        accuracy = max(0, 100 - (20 * deviation))
        return accuracy
    
    merged_data["accuracy_pct"] = merged_data.apply(calculate_accuracy_percentage, axis=1)
    
    print(f"Final merged data size: {len(merged_data)} rows")
    print(f"Average absolute prediction error: {merged_data['ape'].mean():.2f} minutes")
    print(f"Average accuracy percentage: {merged_data['accuracy_pct'].mean():.2f}%")
    
    return merged_data

# Generate tables for the analysis
def generate_tables(merged_data):
    tables = {}
    
    # Table 11: Overall Prediction Accuracy Metrics by Metro Line
    table_11 = merged_data.groupby("lineId").agg(
        Observations=("ape", "count"),
        MAE=("ape", "mean"),
        RMSE=("ape", lambda x: np.sqrt(np.mean(x**2))),
        Prediction_Accuracy_Rate=("accuracy_pct", "mean"),
        Directional_Bias=("bias", "mean")
    ).reset_index()
    
    # Add system average
    system_avg = pd.DataFrame({
        "lineId": ["System Average"],
        "Observations": [merged_data["ape"].count()],
        "MAE": [merged_data["ape"].mean()],
        "RMSE": [np.sqrt(np.mean(merged_data["ape"]**2))],
        "Prediction_Accuracy_Rate": [merged_data["accuracy_pct"].mean()],
        "Directional_Bias": [merged_data["bias"].mean()]
    })
    
    table_11 = pd.concat([table_11, system_avg], ignore_index=True)
    tables["table_11"] = table_11
    
    # Table 12: Prediction Accuracy by Time Period
    # Calculate metrics for detailed (timePeriod, dayType: weekday, saturday, sunday)
    # No period-specific or overall averages are included here for Figure 3 as per user request.
    table_12 = merged_data.groupby(["timePeriod", "dayType"]).agg(
        MAE=("ape", "mean"),
        Prediction_Accuracy_Rate=("accuracy_pct", "mean"),
        Directional_Bias=("bias", "mean"),
        Observations=("ape", "count")
    ).reset_index()
    
    tables["table_12"] = table_12
    
    # Table 13: Prediction Accuracy by Station Type
    table_13 = merged_data.groupby("stationType").agg(
        MAE=("ape", "mean"),
        Prediction_Accuracy_Rate=("accuracy_pct", "mean"),
        Observations=("ape", "count"),
        Stations_Sampled=("stationId", "nunique")
    ).reset_index()
    tables["table_13"] = table_13
    
    # Table 14: Distribution of Prediction Errors (System-wide)
    # Define error ranges
    bins = [-float('inf'), -5, -3, -1, 1, 3, 5, float('inf')]
    labels = ['< -5 min', '-5 to -3 min', '-3 to -1 min', '-1 to +1 min', '+1 to +3 min', '+3 to +5 min', '> +5 min']
    
    merged_data['error_range'] = pd.cut(merged_data['bias'], bins=bins, labels=labels)
    
    table_14 = merged_data['error_range'].value_counts().reset_index()
    table_14.columns = ['Error Range', 'Frequency']
    table_14['Percentage'] = 100 * table_14['Frequency'] / table_14['Frequency'].sum()
    table_14['Cumulative Percentage'] = table_14['Percentage'].cumsum()
    
    # Sort by the defined order
    error_range_order = {label: i for i, label in enumerate(labels)}
    table_14['order'] = table_14['Error Range'].map(error_range_order)
    table_14 = table_14.sort_values('order').drop('order', axis=1)
    
    tables["table_14"] = table_14
    
    # Table 16: Multiple Regression Analysis of Factors Affecting Prediction Error
    # Using statsmodels.api.OLS for proper statistical output
    
    # Prepare independent variables (X)
    # Create dummy variables for categorical features
    X_categorical = pd.get_dummies(merged_data[['stationType', 'timePeriod', 'dayType', 'passengerVolume', 'weather']], drop_first=True)
    
    X = X_categorical # Using the dummy variables directly
    
    # Add a constant (intercept) to the model, as statsmodels doesn't add it by default
    X_sm = sm.add_constant(X)
    
    # Define the dependent variable (y)
    y = merged_data['ape']
    
    # Fit the OLS model
    # Ensure y and X_sm have consistent indices and are numeric.
    # Using .astype(float) for X_sm columns to prevent potential dtype issues with statsmodels.
    model_sm = sm.OLS(y, X_sm.astype(float)).fit()
    
    # Create a DataFrame for regression results table
    table_16_df = pd.DataFrame({
        'Predictor Variable': X_sm.columns,
        'Coefficient': model_sm.params,
        'Standard Error': model_sm.bse,
        't-value': model_sm.tvalues,
        'p-value': model_sm.pvalues
    }).reset_index(drop=True) # Ensure a clean index

    # Add significance indicators
    # Defined locally to keep it contained to this table's logic
    def add_significance_sm(p_val):
        if p_val < 0.001:
            return '***'
        elif p_val < 0.01:
            return '**'
        elif p_val < 0.05:
            return '*'
        else:
            return ''

    table_16_df['Significance'] = table_16_df['p-value'].apply(add_significance_sm)
    
    tables["table_16"] = table_16_df
    
    # Store model summary information using statsmodels attributes
    table_16_info_dict = {
        'R-squared': model_sm.rsquared,
        'Adjusted R-squared': model_sm.rsquared_adj,
        'F-statistic': model_sm.fvalue,
        'p-value (F-statistic)': model_sm.f_pvalue,
        'Number of Observations': model_sm.nobs,
        'Log-Likelihood': model_sm.llf,
        'AIC': model_sm.aic,
        'BIC': model_sm.bic
    }
    tables["table_16_info"] = table_16_info_dict
    
    return tables

# Function to process survey data for comparison with objective measurements
def process_survey_data(survey_data, merged_data):
    # Extract line-specific accuracy ratings from survey
    # Based on DEM_3 (which lines users use) and SAT_2 (perception of accuracy)
    
    # Process DEM_3 to extract which lines each respondent uses
    def extract_lines(dem3_value):
        if not isinstance(dem3_value, str):
            return []
        
        # Format is like "3_1,2,4" where 1=A1, 2=M1, 3=M4, 4=B1
        try:
            options = dem3_value.split('_')[1].split(',')
            line_map = {'1': 'A1_Line', '2': 'M1_Line', '3': 'M4_Line', '4': 'B1_Line'}
            return [line_map.get(opt, '') for opt in options if opt in line_map]
        except (IndexError, AttributeError):
            return []
    
    # Apply the extraction function
    survey_data['used_lines'] = survey_data['DEM_3'].apply(extract_lines)
    
    # Explode the data so each row represents one respondent-line combination
    exploded_data = survey_data.explode('used_lines').dropna(subset=['used_lines'])
    
    # Get accuracy rating (SAT_2 option 3 represents "Tahminlerin doğruluğu")
    def has_accuracy_selected(sat2_value):
        if not isinstance(sat2_value, str):
            return False
        
        try:
            options = sat2_value.split('_')[1].split(',')
            return '3' in options
        except (IndexError, AttributeError):
            return False
    
    exploded_data['values_accuracy'] = exploded_data['SAT_2'].apply(has_accuracy_selected)
    
    # Get general satisfaction with the app (SAT_1)
    exploded_data['app_satisfaction'] = pd.to_numeric(exploded_data['SAT_1'], errors='coerce')
    
    # Aggregate by line
    line_perception = exploded_data.groupby('used_lines').agg(
        accuracy_rating=('values_accuracy', lambda x: x.mean() * 5),  # Scale to 1-5
        app_satisfaction=('app_satisfaction', 'mean'),
        pct_satisfied=('app_satisfaction', lambda x: (x >= 4).mean() * 100),  # % rating 4 or 5
        respondents=('Response ID', 'count')
    ).reset_index()
    
    line_perception = line_perception.rename(columns={'used_lines': 'lineId'})
    
    # Calculate objective metrics by line
    objective_metrics = merged_data.groupby('lineId').agg(
        mae=('ape', 'mean'),
        par=('accuracy_pct', 'mean')
    ).reset_index()
    
    # Merge perception with objective metrics
    comparison_data = pd.merge(line_perception, objective_metrics, on='lineId', how='left')
    
    # Calculate perception gap
    comparison_data['perception_gap'] = comparison_data['accuracy_rating'] - (5 - comparison_data['mae'])  # Transform MAE to 1-5 scale
    
    return comparison_data

# Create visualizations
def create_visualizations(tables, merged_data, comparison_data):
    visualizations = {}
    
    # Apply a style for academic-looking plots
    plt.style.use('seaborn-v0_8-paper')

    # Define branded colors for transit lines
    line_colors = {
        'M1_Line': '#E30613',  # Red for Sincan Koru Metro (M1)
        'M4_Line': '#F4B233',  # Yellow/Gold for Keçiören Metro (M4)
        'A1_Line': '#007C30',  # Green for Ankaray (A1)
        'B1_Line': '#009B9E',  # Teal/Turquoise for Başkentray (B1)
        'default': '#555555'   # Dark gray for any other lines/default
    }
    
    # Figure 1: Overall Prediction Accuracy by Metro Line
    plt.figure(figsize=(10, 6))
    fig1_data = tables["table_11"][tables["table_11"]["lineId"] != "System Average"]
    fig1_bar_colors = [line_colors.get(line_id, line_colors['default']) for line_id in fig1_data['lineId']]
    sns.barplot(x='lineId', y='MAE', data=fig1_data, palette=fig1_bar_colors, hue='lineId', legend=False) # Use line_colors
    plt.title('Mean Absolute Error (MAE) by Metro Line', fontsize=14) # Ensure normal weight
    plt.xlabel('Metro Line', fontsize=12)
    plt.ylabel('MAE (minutes)', fontsize=12)
    plt.grid(axis='y', linestyle='-', alpha=0.5, color='gray') # Updated grid
    plt.tight_layout()
    plt.savefig(output_dir / "fig1_mae_by_line.png", dpi=300)
    visualizations["fig1"] = "MAE by Metro Line"
    
    # Figure 2: Prediction Accuracy Rate by Metro Line
    plt.figure(figsize=(10, 6))
    fig2_data = tables["table_11"][tables["table_11"]["lineId"] != "System Average"]
    fig2_bar_colors = [line_colors.get(line_id, line_colors['default']) for line_id in fig2_data['lineId']]
    sns.barplot(x='lineId', y='Prediction_Accuracy_Rate', data=fig2_data, palette=fig2_bar_colors, hue='lineId', legend=False) # Use line_colors
    plt.title('Prediction Accuracy Rate by Metro Line', fontsize=14) # Ensure normal weight
    plt.xlabel('Metro Line', fontsize=12)
    plt.ylabel('Prediction Accuracy Rate (%)', fontsize=12)
    plt.grid(axis='y', linestyle='-', alpha=0.5, color='gray') # Updated grid
    plt.tight_layout()
    plt.savefig(output_dir / "fig2_accuracy_rate_by_line.png", dpi=300)
    visualizations["fig2"] = "Accuracy Rate by Metro Line"
    
    # Figure 3: MAE by Time Period and Day Type
    plt.figure(figsize=(12, 7))
    time_period_data_fig3 = tables["table_12"].copy() 
    
    # Correctly map dayType to day_group for hue (Weekday/Weekend only)
    def map_day_type_to_group_fig3(day_type):
        if day_type == 'weekday':
            return 'Weekday'
        elif day_type in ['saturday', 'sunday']:
            return 'Weekend'
        return day_type # Fallback, should not be hit if data is clean

    time_period_data_fig3.loc[:, 'day_group'] = time_period_data_fig3['dayType'].apply(map_day_type_to_group_fig3)
    
    # Filter out any rows that didn't map to Weekday or Weekend for robustness
    time_period_data_fig3 = time_period_data_fig3[time_period_data_fig3['day_group'].isin(['Weekday', 'Weekend'])]

    # Define the order for time periods (x-axis) and for hues
    time_period_order_fig3 = ['Morning Peak', 'Midday Off-Peak', 'Evening Peak', 'Evening Off-Peak']
    hue_order_fig3 = ['Weekday', 'Weekend'] 
    
    ax3 = sns.barplot(x='timePeriod', y='MAE', hue='day_group', data=time_period_data_fig3, 
                    order=time_period_order_fig3, hue_order=hue_order_fig3, palette='Set2')
    plt.title('Mean Absolute Error (MAE) by Time Period', fontsize=14) 
    plt.xlabel('Time Period', fontsize=12)
    plt.ylabel('MAE (minutes)', fontsize=12)
    ax3.grid(axis='y', linestyle='-', alpha=0.5, color='gray') # Updated grid
    plt.legend(title='Day Type', fontsize='medium')
    plt.tight_layout()
    plt.savefig(output_dir / "fig3_mae_by_time_period.png", dpi=300)
    visualizations["fig3"] = "MAE by Time Period"
    
    # Figure 4: Prediction Accuracy by Station Type
    plt.figure(figsize=(10, 6))
    station_type_order = ['Terminal', 'Transfer', 'Standard']
    # Use a consistent color palette if not using line_colors directly
    ax4 = sns.barplot(x='stationType', y='MAE', data=tables["table_13"], order=station_type_order, hue='stationType', palette='viridis', legend=False)
    
    for i, row in enumerate(tables["table_13"].itertuples()):
        if row.stationType in station_type_order:
            # Ensure ax4 is used if it's the correct axes object for text
            ax4.text(station_type_order.index(row.stationType), row.MAE + 0.1, 
                   f"n={row.Stations_Sampled}", ha='center', fontsize=10)
    
    plt.title('Mean Absolute Error (MAE) by Station Type', fontsize=14) # Ensure normal weight
    plt.xlabel('Station Type', fontsize=12)
    plt.ylabel('MAE (minutes)', fontsize=12)
    ax4.grid(axis='y', linestyle='-', alpha=0.5, color='gray') # Updated grid
    plt.tight_layout()
    plt.savefig(output_dir / "fig4_mae_by_station_type.png", dpi=300)
    visualizations["fig4"] = "MAE by Station Type"
    
    # Figure 5: Distribution of Prediction Errors
    plt.figure(figsize=(12, 6))
    error_range_order = tables["table_14"]['Error Range'].tolist()
    
    ax5 = sns.barplot(x='Error Range', y='Percentage', data=tables["table_14"], 
                    order=error_range_order, hue='Error Range', palette='viridis', legend=False)
    
    ax5_twin = ax5.twinx()
    cumulative_data = tables["table_14"].set_index('Error Range').loc[error_range_order, 'Cumulative Percentage']
    ax5_twin.plot(range(len(error_range_order)), cumulative_data, 'o-', color='tab:red', linewidth=2, markersize=8) # Matched color from original image
    ax5_twin.set_ylabel('Cumulative Percentage (%)', color='tab:red', fontsize=12)
    ax5_twin.tick_params(axis='y', colors='tab:red', labelsize=10)
    
    plt.title('Distribution of Prediction Errors (System-wide)', fontsize=14) # Ensure normal weight
    plt.xlabel('Error Range', fontsize=12)
    ax5.set_ylabel('Percentage (%)', fontsize=12)
    ax5.grid(axis='y', linestyle='-', alpha=0.5, color='gray') # Updated grid for primary axis
    ax5_twin.grid(False) # Ensure secondary axis has no grid
    plt.tight_layout()
    plt.savefig(output_dir / "fig5_error_distribution.png", dpi=300)
    visualizations["fig5"] = "Error Distribution"
    
    # Figure 6: Objective vs Perceived Accuracy by Line
    comparison_plot_data = comparison_data.copy()
    
    fig6, ax1 = plt.subplots(figsize=(10, 6))
    
    bar_colors = [line_colors.get(line_id, line_colors['default']) for line_id in comparison_plot_data['lineId']]
    
    ax1.bar(comparison_plot_data['lineId'], comparison_plot_data['mae'], color=bar_colors, alpha=0.8, label='_nolegend_')
    ax1.set_xlabel('Metro Line', fontsize=12)
    ax1.set_ylabel('Mean Absolute Error (MAE)', color='black', fontsize=12)
    ax1.tick_params(axis='y', labelcolor='black', labelsize=10)
    ax1.tick_params(axis='x', labelsize=10)

    ax2 = ax1.twinx()
    perceived_accuracy_color = '#0072B2' 
    ax2.plot(comparison_plot_data['lineId'], comparison_plot_data['accuracy_rating'], 'o-', color=perceived_accuracy_color, 
            linewidth=2.5, markersize=8, label='Perceived Accuracy')
    ax2.set_ylabel('Perceived Accuracy (1-5)', color=perceived_accuracy_color, fontsize=12)
    ax2.tick_params(axis='y', labelcolor=perceived_accuracy_color, labelsize=10)
    
    ax1.set_title('Objective Accuracy vs. User Perception by Line', fontsize=14) 
    
    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines1 + lines2, labels1 + labels2, loc='upper center', 
               bbox_to_anchor=(0.5, -0.15), fancybox=True, shadow=False, ncol=2, fontsize='medium')
    
    ax1.grid(axis='y', linestyle='-', alpha=0.5, color='gray') # Updated grid for primary axis
    ax2.grid(False) 

    fig6.tight_layout(rect=[0, 0.05, 1, 0.95]) 
    
    plt.savefig(output_dir / "fig6_objective_vs_perceived.png", dpi=300)
    visualizations["fig6"] = "Objective vs Perceived Accuracy"
    
    return visualizations

# Save tables to CSV
def save_tables(tables):
    for name, table in tables.items():
        if isinstance(table, pd.DataFrame):
            table.to_csv(output_dir / f"{name}.csv", index=False)

# Main function
def main():
    print("Loading data...")
    schedule_data = load_schedule_data()
    record_data = load_record_data()
    survey_data = load_survey_data()
    
    print("Analyzing arrival accuracy...")
    merged_data = analyze_arrival_accuracy(record_data, schedule_data)
    
    print("Generating analysis tables...")
    tables = generate_tables(merged_data)
    
    print("Processing survey data for comparison...")
    comparison_data = process_survey_data(survey_data, merged_data)
    
    # Add comparison data to tables
    tables["table_17"] = comparison_data
    
    print("Creating visualizations...")
    visualizations = create_visualizations(tables, merged_data, comparison_data)
    
    print("Saving tables to CSV...")
    save_tables(tables)
    
    print("Analysis complete!")
    return tables, visualizations

if __name__ == "__main__":
    main()
