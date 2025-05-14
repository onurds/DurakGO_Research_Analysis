import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
from datetime import datetime
from scipy import stats
import re

# Set plot style
plt.style.use('seaborn-v0_8-whitegrid')
sns.set_palette("viridis")
plt.rcParams['figure.figsize'] = (12, 8)
plt.rcParams['font.size'] = 12


def load_survey_data(file_path):
    """
    Load and prepare survey data for exploratory analysis
    """
    # Check if file exists
    if not os.path.exists(file_path):
        print(f"File not found: {file_path}")
        return None

    # Load the data
    print(f"Loading data from: {file_path}")
    df = pd.read_csv(file_path)

    # Basic data checks
    print(f"Loaded {df.shape[0]} rows and {df.shape[1]} columns")
    return df


def create_output_directory():
    """
    Create directory for EDA outputs
    """
    output_dir = "Survey_Results/Analysis Results/EDA_Results"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    print(f"Created output directory: {output_dir}")
    return output_dir


def describe_survey_metadata(df, output_dir):
    """
    Analyze and visualize metadata about the survey responses
    """
    print("Analyzing survey metadata...")
    
    # Get basic metadata
    metadata = {
        'Total Responses': len(df),
        'Completion Rate': df['Finished'].value_counts(normalize=True).get('Yes', 0) * 100,
        'Unique Devices': df['userAgent - device'].nunique(),
        'Unique OS': df['userAgent - os'].nunique(),
        'Unique Browsers': df['userAgent - browser'].nunique()
    }
    
    # Create metadata summary table
    metadata_df = pd.DataFrame.from_dict(metadata, orient='index', columns=['Value'])
    metadata_df.to_csv(f"{output_dir}/survey_metadata_summary.csv")
    
    # Device distribution - use horizontal bar chart instead
    plt.figure(figsize=(10, 6))
    device_counts = df['userAgent - device'].value_counts()
    plt.barh(device_counts.index, device_counts.values, color=sns.color_palette("viridis", len(device_counts)))
    plt.title('Distribution of Survey Responses by Device Type')
    plt.ylabel('Device Type')
    plt.xlabel('Number of Responses')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/device_distribution.png", dpi=300)
    plt.close()
    
    # OS distribution - use donut chart
    plt.figure(figsize=(10, 8))
    os_counts = df['userAgent - os'].value_counts()
    colors = sns.color_palette("viridis", len(os_counts))
    
    # Create a donut chart
    plt.pie(os_counts.values, labels=os_counts.index, autopct='%1.1f%%', 
            colors=colors, wedgeprops=dict(width=0.5, edgecolor='w'))
    plt.title('Distribution of Survey Responses by Operating System')
    plt.axis('equal')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/os_distribution.png", dpi=300)
    plt.close()
    
    # Create summary table of metadata distributions
    os_dist = df['userAgent - os'].value_counts(normalize=True).reset_index()
    os_dist.columns = ['OS', 'Percentage']
    os_dist['Percentage'] = os_dist['Percentage'] * 100
    
    device_dist = df['userAgent - device'].value_counts(normalize=True).reset_index()
    device_dist.columns = ['Device', 'Percentage']
    device_dist['Percentage'] = device_dist['Percentage'] * 100
    
    browser_dist = df['userAgent - browser'].value_counts(normalize=True).reset_index()
    browser_dist.columns = ['Browser', 'Percentage']
    browser_dist['Percentage'] = browser_dist['Percentage'] * 100
    
    # Save distribution tables
    os_dist.to_csv(f"{output_dir}/os_distribution.csv", index=False)
    device_dist.to_csv(f"{output_dir}/device_distribution.csv", index=False)
    browser_dist.to_csv(f"{output_dir}/browser_distribution.csv", index=False)


def decode_multiple_choice(col, pattern_prefix):
    """Helper function to decode multiple choice fields with category codes"""
    if pd.isna(col):
        return []
    
    # Extract numbers after the prefix
    match = re.match(f"{pattern_prefix}_(.+)", str(col))
    if not match:
        return []
        
    # Split the numbers by comma
    choices = match.group(1).split(',')
    return [int(choice) for choice in choices]


def prepare_categorical_data(df):
    """
    Prepare categorical data for analysis by creating expanded multiple-choice fields
    """
    print("Preparing categorical data...")
    
    # Handle multiple choice fields
    # USE_3: When users typically use DurakGO
    use3_options = {
        1: "Before_leaving",
        2: "While_going_station", 
        3: "At_station",
        4: "During_disruptions", 
        5: "For_planning"
    }
    
    for option_num, option_name in use3_options.items():
        df[f'USE_3_{option_name}'] = df['USE_3'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '3') else 0
        )
    
    # USE_4: Most used features
    use4_options = {
        1: "Check_arrival_times",
        2: "Find_nearest_stations", 
        3: "View_station_map",
        4: "Search_stations", 
        5: "View_both_directions"
    }
    
    for option_num, option_name in use4_options.items():
        df[f'USE_4_{option_name}'] = df['USE_4'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '4') else 0
        )
    
    # WAIT_5: Actions when DurakGO shows long wait time
    wait5_options = {
        1: "Adjust_arrival_time",
        2: "Choose_different_station", 
        3: "Use_different_transport",
        4: "Do_something_productive", 
        5: "Use_different_route",
        6: "No_behavior_change"
    }
    
    for option_num, option_name in wait5_options.items():
        df[f'WAIT_5_{option_name}'] = df['WAIT_5'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '5') else 0
        )
    
    # SAT_2: Aspects users like about DurakGO
    sat2_options = {
        1: "Accurate_arrival_times",
        2: "Easy_to_use", 
        3: "Clean_design",
        4: "Offline_functionality", 
        5: "Station_map",
        6: "Search_function",
        7: "Other"
    }
    
    for option_num, option_name in sat2_options.items():
        df[f'SAT_2_{option_name}'] = df['SAT_2'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '2') else 0
        )
    
    # SAT_3: Areas for improvement
    sat3_options = {
        1: "More_accurate_times",
        2: "Faster_updates", 
        3: "Better_offline_mode",
        4: "Improved_UI", 
        5: "More_stations",
        6: "Better_search",
        7: "Other"
    }
    
    for option_num, option_name in sat3_options.items():
        df[f'SAT_3_{option_name}'] = df['SAT_3'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '3') else 0
        )
    
    # SAT_4: Desired features
    sat4_options = {
        1: "Trip_planning",
        2: "Service_alerts", 
        3: "Personalization",
        4: "More_transport_modes", 
        5: "Push_notifications"
    }
    
    for option_num, option_name in sat4_options.items():
        df[f'SAT_4_{option_name}'] = df['SAT_4'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '4') else 0
        )
    
    # DEM_3: Most used lines
    dem3_options = {
        1: "Ankaray",
        2: "M1", 
        3: "M2",
        4: "M3", 
        5: "M4"
    }
    
    for option_num, option_name in dem3_options.items():
        df[f'DEM_3_{option_name}'] = df['DEM_3'].apply(
            lambda x: 1 if pd.notna(x) and str(option_num) in decode_multiple_choice(x, '4') else 0
        )
    
    return df


def analyze_demographics(df, output_dir):
    """
    Analyze and visualize demographic information
    """
    print("Analyzing demographic data...")
    
    # Create mapping dictionaries for demographic variables
    age_mapping = {
        1: "Under 18",
        2: "18-24",
        3: "25-34",
        4: "35-44",
        5: "45-54",
        6: "55-64",
        7: "65-74",
        8: "75+"
    }
    
    gender_mapping = {
        1: "Male",
        2: "Female",
        3: "Other",
        4: "Prefer not to say"
    }
    
    # 1. Age distribution - use a horizontal bar chart with percentage
    plt.figure(figsize=(10, 6))
    age_counts = df['DEM_5'].value_counts().sort_index()
    age_labels = [age_mapping.get(i, f"Unknown ({i})") for i in age_counts.index]
    
    # Calculate percentages
    percentages = age_counts / age_counts.sum() * 100
    
    # Create a horizontal bar chart
    plt.barh(age_labels, percentages, color=sns.color_palette("viridis", len(age_counts)))
    plt.title('Age Distribution of Survey Respondents')
    plt.ylabel('Age Group')
    plt.xlabel('Percentage (%)')
    
    # Add percentage labels to the bars
    for i, v in enumerate(percentages):
        plt.text(v + 0.5, i, f"{v:.1f}%", va='center')
    
    plt.tight_layout()
    plt.savefig(f"{output_dir}/age_distribution.png", dpi=300)
    plt.close()
    
    # 2. Gender distribution - use a pie chart with custom colors
    plt.figure(figsize=(10, 8))
    gender_counts = df['DEM_6'].value_counts().sort_index()
    gender_labels = [gender_mapping.get(i, f"Unknown ({i})") for i in gender_counts.index]
    
    # Custom gender-neutral colors
    colors = sns.color_palette("Pastel1", len(gender_counts))
    
    # Create pie chart with a slight explosion for the largest segment
    explode = [0] * len(gender_counts)
    if len(gender_counts) > 0:
        explode[gender_counts.values.argmax()] = 0.1
    
    plt.pie(gender_counts.values, labels=gender_labels, autopct='%1.1f%%', 
            colors=colors, explode=explode, shadow=True, startangle=90)
    plt.title('Gender Distribution of Survey Respondents')
    plt.axis('equal')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/gender_distribution.png", dpi=300)
    plt.close()
    
    # 3. Metro usage frequency - use a grouped bar chart
    usage_mapping = {
        1: "Daily",
        2: "2-3 times per week",
        3: "Once a week",
        4: "2-3 times per month",
        5: "Once a month or less"
    }
    
    plt.figure(figsize=(12, 6))
    usage_counts = df['DEM_1'].value_counts().sort_index()
    usage_labels = [usage_mapping.get(i, f"Unknown ({i})") for i in usage_counts.index]
    
    # Calculate counts and percentages
    counts = usage_counts.values
    percentages = counts / counts.sum() * 100
    
    # Create a figure with two subplots side by side
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
    
    # Plot counts on the left subplot
    bars1 = ax1.bar(usage_labels, counts, color=sns.color_palette("viridis", len(counts)))
    ax1.set_title('Metro Usage Frequency (Counts)')
    ax1.set_xlabel('Frequency')
    ax1.set_ylabel('Number of Respondents')
    ax1.tick_params(axis='x', rotation=45)
    
    # Add count labels
    for bar in bars1:
        height = bar.get_height()
        ax1.text(bar.get_x() + bar.get_width()/2., height + 0.1,
                f'{int(height)}', ha='center', va='bottom')
    
    # Plot percentages on the right subplot
    bars2 = ax2.bar(usage_labels, percentages, color=sns.color_palette("viridis", len(percentages)))
    ax2.set_title('Metro Usage Frequency (Percentages)')
    ax2.set_xlabel('Frequency')
    ax2.set_ylabel('Percentage (%)')
    ax2.tick_params(axis='x', rotation=45)
    
    # Add percentage labels
    for bar in bars2:
        height = bar.get_height()
        ax2.text(bar.get_x() + bar.get_width()/2., height + 0.5,
                f'{height:.1f}%', ha='center', va='bottom')
    
    plt.tight_layout()
    plt.savefig(f"{output_dir}/metro_usage_frequency.png", dpi=300)
    plt.close()
    
    # 4. Primary transportation mode - use a horizontal lollipop chart
    transport_mapping = {
        1: "Metro/Rail",
        2: "Bus",
        3: "Car",
        4: "Walking",
        5: "Bicycle/Scooter"
    }
    
    plt.figure(figsize=(10, 6))
    transport_counts = df['DEM_2'].value_counts().sort_values(ascending=True)
    transport_labels = [transport_mapping.get(i, f"Unknown ({i})") for i in transport_counts.index]
    
    # Create lollipop chart
    plt.hlines(y=range(len(transport_counts)), xmin=0, xmax=transport_counts.values, color='skyblue')
    plt.plot(transport_counts.values, range(len(transport_counts)), "o", markersize=10, color='dodgerblue')
    
    # Add count labels
    for i, v in enumerate(transport_counts.values):
        plt.text(v + 0.1, i, str(v), va='center')
    
    plt.yticks(range(len(transport_counts)), transport_labels)
    plt.title('Primary Transportation Mode')
    plt.xlabel('Number of Respondents')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/primary_transport_mode.png", dpi=300)
    plt.close()
    
    # Create a summary table of demographic distributions
    demographic_summary = {
        'Age Group': df['DEM_5'].map(age_mapping).value_counts(normalize=True) * 100,
        'Gender': df['DEM_6'].map(gender_mapping).value_counts(normalize=True) * 100,
        'Metro Usage Frequency': df['DEM_1'].map(usage_mapping).value_counts(normalize=True) * 100,
        'Primary Transport Mode': df['DEM_2'].map(transport_mapping).value_counts(normalize=True) * 100
    }
    
    # Save each demographic summary to CSV
    for category, distribution in demographic_summary.items():
        distribution_df = pd.DataFrame(distribution)
        distribution_df.columns = ['Percentage']
        distribution_df.index.name = category
        distribution_df.to_csv(f"{output_dir}/{category.lower().replace(' ', '_')}_distribution.csv")


def analyze_usage_patterns(df, output_dir):
    """
    Analyze and visualize app usage patterns
    """
    print("Analyzing usage patterns...")
    
    # Create mapping dictionaries for usage variables
    usage_time_mapping = {
        1: "Less than 1 week",
        2: "1-4 weeks",
        3: "1-3 months",
        4: "More than 3 months"
    }
    
    usage_freq_mapping = {
        1: "Multiple times per day",
        2: "Once per day",
        3: "Several times per week",
        4: "Once per week or less"
    }
    
    offline_usage_mapping = {
        1: "Yes, frequently",
        2: "Yes, occasionally",
        3: "No, always have internet",
        4: "No, didn't know it worked offline"
    }
    
    # 1. Length of time using app
    plt.figure(figsize=(10, 6))
    time_counts = df['USE_1'].value_counts().sort_index()
    time_labels = [usage_time_mapping.get(i, f"Unknown ({i})") for i in time_counts.index]
    
    sns.barplot(x=time_labels, y=time_counts.values)
    plt.title('How Long Users Have Been Using DurakGO')
    plt.xlabel('Time Period')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/app_usage_time.png", dpi=300)
    plt.close()
    
    # 2. App usage frequency
    plt.figure(figsize=(10, 6))
    freq_counts = df['USE_2'].value_counts().sort_index()
    freq_labels = [usage_freq_mapping.get(i, f"Unknown ({i})") for i in freq_counts.index]
    
    sns.barplot(x=freq_labels, y=freq_counts.values)
    plt.title('DurakGO Usage Frequency')
    plt.xlabel('Frequency')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/app_usage_frequency.png", dpi=300)
    plt.close()
    
    # 3. Offline mode usage
    plt.figure(figsize=(10, 6))
    offline_counts = df['USE_5'].value_counts().sort_index()
    offline_labels = [offline_usage_mapping.get(i, f"Unknown ({i})") for i in offline_counts.index]
    
    plt.pie(offline_counts.values, labels=offline_labels, autopct='%1.1f%%')
    plt.title('Offline Mode Usage')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/offline_mode_usage.png", dpi=300)
    plt.close()
    
    # 4. When users use the app (multiple choice)
    use_time_cols = [col for col in df.columns if col.startswith('USE_3_')]
    use_time_data = df[use_time_cols].sum().sort_values(ascending=False)
    
    plt.figure(figsize=(12, 6))
    sns.barplot(x=use_time_data.index, y=use_time_data.values)
    plt.title('When Users Typically Use DurakGO')
    plt.xlabel('Usage Scenario')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/app_usage_scenarios.png", dpi=300)
    plt.close()
    
    # 5. Most used features (multiple choice)
    features_cols = [col for col in df.columns if col.startswith('USE_4_')]
    features_data = df[features_cols].sum().sort_values(ascending=False)
    
    plt.figure(figsize=(12, 6))
    sns.barplot(x=features_data.index, y=features_data.values)
    plt.title('Most Used DurakGO Features')
    plt.xlabel('Feature')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/most_used_features.png", dpi=300)
    plt.close()
    
    # Save usage patterns summary to CSV
    usage_summary = {
        'Length of Time Using App': df['USE_1'].map(usage_time_mapping).value_counts(normalize=True) * 100,
        'App Usage Frequency': df['USE_2'].map(usage_freq_mapping).value_counts(normalize=True) * 100,
        'Offline Mode Usage': df['USE_5'].map(offline_usage_mapping).value_counts(normalize=True) * 100
    }
    
    for category, distribution in usage_summary.items():
        distribution_df = pd.DataFrame(distribution)
        distribution_df.columns = ['Percentage']
        distribution_df.index.name = category
        distribution_df.to_csv(f"{output_dir}/{category.lower().replace(' ', '_')}_distribution.csv")
    
    # Save multiple choice summaries
    pd.DataFrame({'Count': use_time_data}).to_csv(f"{output_dir}/usage_scenarios_summary.csv")
    pd.DataFrame({'Count': features_data}).to_csv(f"{output_dir}/used_features_summary.csv")


def analyze_wait_time_impact(df, output_dir):
    """
    Analyze and visualize the impact of DurakGO on wait times and wait experience
    """
    print("Analyzing wait time impact...")
    
    # Create mapping dictionaries for wait time variables
    wait_mapping = {
        1: "Less than 3 minutes",
        2: "3-5 minutes",
        3: "6-10 minutes",
        4: "11-15 minutes",
        5: "More than 15 minutes"
    }
    
    wait_experience_mapping = {
        5: "Feels much shorter",
        4: "Feels a bit shorter",
        3: "No effect",
        2: "Feels a bit longer",
        1: "Feels much longer"
    }
    
    relaxation_mapping = {
        5: "Much more relaxed",
        4: "Somewhat more relaxed",
        3: "No difference",
        2: "Somewhat more anxious",
        1: "Much more anxious"
    }
    
    # 1. Perceived wait times before and after using DurakGO - use stacked percentage bar chart
    wait_before = df['WAIT_1'].value_counts(normalize=True).sort_index() * 100
    wait_after = df['WAIT_2'].value_counts(normalize=True).sort_index() * 100
    
    plt.figure(figsize=(14, 8))
    
    # Create a DataFrame for easier plotting
    wait_df = pd.DataFrame({
        'Before DurakGO': [wait_before.get(i, 0) for i in range(1, 6)],
        'With DurakGO': [wait_after.get(i, 0) for i in range(1, 6)]
    }, index=[wait_mapping.get(i, f"Unknown ({i})") for i in range(1, 6)])
    
    # Plot as stacked percentage bars
    ax = wait_df.plot(kind='bar', stacked=False, figsize=(14, 8), 
                     color=['#ff9999', '#66b3ff'], width=0.7)
    
    # Add percentage labels on the bars
    for c in ax.containers:
        ax.bar_label(c, fmt='%.1f%%', label_type='edge')
    
    plt.xlabel('Perceived Wait Time')
    plt.ylabel('Percentage of Responses (%)')
    plt.title('Perceived Wait Times Before and After Using DurakGO')
    plt.legend(title='Survey Condition')
    plt.grid(axis='y', linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/wait_time_comparison.png", dpi=300)
    plt.close()
    
    # Calculate average change in perceived wait time
    wait_time_diff = []
    for _, row in df.iterrows():
        if pd.notna(row['WAIT_1']) and pd.notna(row['WAIT_2']):
            wait_time_diff.append(row['WAIT_1'] - row['WAIT_2'])
    
    avg_wait_reduction = np.mean(wait_time_diff)
    median_wait_reduction = np.median(wait_time_diff)
    
    wait_reduction_summary = pd.DataFrame({
        'Metric': ['Average Perceived Wait Time Reduction', 'Median Perceived Wait Time Reduction'],
        'Value': [avg_wait_reduction, median_wait_reduction]
    })
    wait_reduction_summary.to_csv(f"{output_dir}/wait_time_reduction_summary.csv", index=False)
    
    # Create a histogram of wait time differences
    plt.figure(figsize=(10, 6))
    plt.hist(wait_time_diff, bins=range(-2, 5), color='skyblue', edgecolor='black', alpha=0.7)
    plt.axvline(x=avg_wait_reduction, color='red', linestyle='--', 
                label=f'Mean: {avg_wait_reduction:.2f}')
    plt.axvline(x=median_wait_reduction, color='green', linestyle='-', 
                label=f'Median: {median_wait_reduction:.2f}')
    plt.title('Distribution of Wait Time Reduction')
    plt.xlabel('Wait Time Reduction (categories)')
    plt.ylabel('Number of Respondents')
    plt.legend()
    plt.grid(axis='y', linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/wait_time_reduction_distribution.png", dpi=300)
    plt.close()
    
    # 2. Wait experience with DurakGO - use a diverging bar chart
    plt.figure(figsize=(12, 6))
    experience_counts = df['WAIT_3'].value_counts().sort_index(ascending=False)
    experience_labels = [wait_experience_mapping.get(i, f"Unknown ({i})") for i in experience_counts.index]
    
    # Create a colormap that diverges from a neutral center
    colors = []
    for idx in experience_counts.index:
        if idx > 3:  # Better experience (positive)
            colors.append('#2ecc71')  # Green
        elif idx < 3:  # Worse experience (negative)
            colors.append('#e74c3c')  # Red
        else:  # Neutral
            colors.append('#95a5a6')  # Gray
    
    # Create diverging bars
    plt.barh(experience_labels, experience_counts.values, color=colors)
    
    # Add count labels
    for i, v in enumerate(experience_counts.values):
        plt.text(v + 0.1, i, str(v), va='center')
    
    plt.title('How DurakGO Affects Wait Experience')
    plt.xlabel('Number of Respondents')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/wait_experience.png", dpi=300)
    plt.close()
    
    # 3. Relaxation effect - use radial chart
    relaxation_counts = df['WAIT_4'].value_counts().sort_index(ascending=False)
    relaxation_labels = [relaxation_mapping.get(i, f"Unknown ({i})") for i in relaxation_counts.index]
    
    # Create data in the right format for radar chart
    categories = relaxation_labels
    N = len(categories)
    values = relaxation_counts.values
    
    # Create angles for each category
    angles = [n / float(N) * 2 * np.pi for n in range(N)]
    angles += angles[:1]  # Close the loop
    
    # Add the first value at the end to close the shape
    values = np.append(values, values[0])
    
    plt.figure(figsize=(10, 8))
    ax = plt.subplot(111, polar=True)
    
    # Draw the plot
    plt.xticks(angles[:-1], categories, size=12)
    ax.plot(angles, values, linewidth=2, linestyle='solid')
    ax.fill(angles, values, 'skyblue', alpha=0.4)
    
    # Add value labels
    for angle, value in zip(angles, values):
        if angle < 2 * np.pi:  # Skip the last repeated point
            plt.text(angle, value + 1, str(int(value)), 
                     ha='center', va='center', size=12)
    
    plt.title('Relaxation Effect of DurakGO', size=15, y=1.1)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/relaxation_effect.png", dpi=300)
    plt.close()
    
    # 4. Actions when DurakGO shows long wait time (multiple choice) - use treemap
    action_cols = [col for col in df.columns if col.startswith('WAIT_5_')]
    action_data = df[action_cols].sum().sort_values(ascending=False)
    
    plt.figure(figsize=(12, 8))
    
    # Clean up the labels for better display
    clean_labels = [label.replace('WAIT_5_', '').replace('_', ' ').title() for label in action_data.index]
    
    # Create a treemap
    # Since treemap is not directly available in matplotlib, simulate with nested squares
    # Or use squarify library if available
    try:
        import squarify
        
        # Compute percentages
        sizes = action_data.values
        total = sum(sizes)
        percentages = [f"{s/total*100:.1f}%" for s in sizes]
        
        # Combine labels with percentages
        labels = [f"{l}\n{p}" for l, p in zip(clean_labels, percentages)]
        
        # Draw the treemap
        squarify.plot(sizes=sizes, label=labels, alpha=0.8, color=sns.color_palette("viridis", len(sizes)))
        plt.axis('off')
        plt.title('Actions Taken When DurakGO Shows Long Wait Time')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/long_wait_actions.png", dpi=300)
        plt.close()
    except ImportError:
        # Fall back to horizontal bar chart if squarify is not available
        plt.barh(clean_labels, action_data.values, color=sns.color_palette("viridis", len(action_data)))
        plt.title('Actions Taken When DurakGO Shows Long Wait Time')
        plt.xlabel('Number of Respondents')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/long_wait_actions.png", dpi=300)
        plt.close()
    
    # Save wait experience summary to CSV
    wait_experience_summary = {
        'Wait Experience': df['WAIT_3'].map(wait_experience_mapping).value_counts(normalize=True) * 100,
        'Relaxation Effect': df['WAIT_4'].map(relaxation_mapping).value_counts(normalize=True) * 100
    }
    
    for category, distribution in wait_experience_summary.items():
        distribution_df = pd.DataFrame(distribution)
        distribution_df.columns = ['Percentage']
        distribution_df.index.name = category
        distribution_df.to_csv(f"{output_dir}/{category.lower().replace(' ', '_')}_distribution.csv")
    
    # Save action data
    pd.DataFrame({'Count': action_data}).to_csv(f"{output_dir}/long_wait_actions_summary.csv")


def analyze_satisfaction_and_impact(df, output_dir):
    """
    Analyze and visualize user satisfaction and app impact
    """
    print("Analyzing user satisfaction and app impact...")
    
    # Create mapping dictionaries
    rating_mapping = {
        1: "1 - Very Poor",
        2: "2 - Poor",
        3: "3 - Average",
        4: "4 - Good",
        5: "5 - Excellent"
    }
    
    transit_usage_change_mapping = {
        5: "Much more frequent",
        4: "Somewhat more frequent",
        3: "No change",
        2: "Somewhat less frequent",
        1: "Much less frequent"
    }
    
    ego_comparison_mapping = {
        5: "Much better",
        4: "Somewhat better",
        3: "About the same",
        2: "Somewhat worse",
        1: "Much worse"
    }
    
    recommendation_mapping = {
        5: "Definitely would",
        4: "Probably would",
        3: "Might or might not",
        2: "Probably would not",
        1: "Definitely would not"
    }
    
    # 1. App Rating Distribution
    plt.figure(figsize=(10, 6))
    app_rating_counts = df['SAT_1'].value_counts().sort_index()
    app_rating_labels = [rating_mapping.get(i, f"Unknown ({i})") for i in app_rating_counts.index]
    
    sns.barplot(x=app_rating_labels, y=app_rating_counts.values)
    plt.title('DurakGO App Rating Distribution')
    plt.xlabel('Rating')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/app_rating_distribution.png", dpi=300)
    plt.close()
    
    # 2. Transit System Rating Distribution
    plt.figure(figsize=(10, 6))
    system_rating_counts = df['IMP_3'].value_counts().sort_index()
    system_rating_labels = [rating_mapping.get(i, f"Unknown ({i})") for i in system_rating_counts.index]
    
    sns.barplot(x=system_rating_labels, y=system_rating_counts.values)
    plt.title("Users' Rating of Ankara's Metro/Rail System")
    plt.xlabel('Rating')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/transit_system_rating_distribution.png", dpi=300)
    plt.close()
    
    # 3. Transit Usage Change Due to DurakGO
    plt.figure(figsize=(10, 6))
    usage_change_counts = df['IMP_1'].value_counts().sort_index()
    usage_change_labels = [transit_usage_change_mapping.get(i, f"Unknown ({i})") for i in usage_change_counts.index]
    
    sns.barplot(x=usage_change_labels, y=usage_change_counts.values)
    plt.title('Change in Transit Usage Due to DurakGO')
    plt.xlabel('Change')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/transit_usage_change.png", dpi=300)
    plt.close()
    
    # 4. Comparison to EGO Cep'te
    plt.figure(figsize=(10, 6))
    ego_comparison_counts = df['IMP_2'].value_counts().sort_index()
    ego_comparison_labels = [ego_comparison_mapping.get(i, f"Unknown ({i})") for i in ego_comparison_counts.index]
    
    sns.barplot(x=ego_comparison_labels, y=ego_comparison_counts.values)
    plt.title('Comparison to EGO Cep\'te App')
    plt.xlabel('Comparison')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/ego_comparison.png", dpi=300)
    plt.close()
    
    # 5. Recommendation Likelihood
    plt.figure(figsize=(10, 6))
    recommendation_counts = df['SAT_5'].value_counts().sort_index()
    recommendation_labels = [recommendation_mapping.get(i, f"Unknown ({i})") for i in recommendation_counts.index]
    
    sns.barplot(x=recommendation_labels, y=recommendation_counts.values)
    plt.title('Likelihood to Recommend DurakGO')
    plt.xlabel('Likelihood')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/recommendation_likelihood.png", dpi=300)
    plt.close()
    
    # 6. What users like about DurakGO (multiple choice)
    like_cols = [col for col in df.columns if col.startswith('SAT_2_')]
    like_data = df[like_cols].sum().sort_values(ascending=False)
    
    plt.figure(figsize=(12, 6))
    sns.barplot(x=like_data.index, y=like_data.values)
    plt.title('What Users Like About DurakGO')
    plt.xlabel('Feature')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/liked_features.png", dpi=300)
    plt.close()
    
    # 7. Areas for improvement (multiple choice)
    improve_cols = [col for col in df.columns if col.startswith('SAT_3_')]
    improve_data = df[improve_cols].sum().sort_values(ascending=False)
    
    plt.figure(figsize=(12, 6))
    sns.barplot(x=improve_data.index, y=improve_data.values)
    plt.title('Areas for Improvement in DurakGO')
    plt.xlabel('Area')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/improvement_areas.png", dpi=300)
    plt.close()
    
    # 8. Desired Features (multiple choice)
    feature_cols = [col for col in df.columns if col.startswith('SAT_4_')]
    feature_data = df[feature_cols].sum().sort_values(ascending=False)
    
    plt.figure(figsize=(12, 6))
    sns.barplot(x=feature_data.index, y=feature_data.values)
    plt.title('Desired Additional Features for DurakGO')
    plt.xlabel('Feature')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/desired_features.png", dpi=300)
    plt.close()
    
    # Calculate Net Promoter Score (NPS)
    if 'SAT_5' in df.columns:
        promoters = len(df[df['SAT_5'] >= 4])
        detractors = len(df[df['SAT_5'] <= 2])
        total_responses = len(df[pd.notna(df['SAT_5'])])
        
        nps = (promoters - detractors) / total_responses * 100
        
        nps_df = pd.DataFrame({
            'Metric': ['Net Promoter Score (NPS)'],
            'Value': [nps]
        })
        nps_df.to_csv(f"{output_dir}/net_promoter_score.csv", index=False)
    
    # Save satisfaction metrics to CSV
    satisfaction_summary = {
        'App Rating': df['SAT_1'].map(rating_mapping).value_counts(normalize=True) * 100,
        'System Rating': df['IMP_3'].map(rating_mapping).value_counts(normalize=True) * 100,
        'Transit Usage Change': df['IMP_1'].map(transit_usage_change_mapping).value_counts(normalize=True) * 100,
        'EGO Comparison': df['IMP_2'].map(ego_comparison_mapping).value_counts(normalize=True) * 100,
        'Recommendation Likelihood': df['SAT_5'].map(recommendation_mapping).value_counts(normalize=True) * 100
    }
    
    for category, distribution in satisfaction_summary.items():
        distribution_df = pd.DataFrame(distribution)
        distribution_df.columns = ['Percentage']
        distribution_df.index.name = category
        distribution_df.to_csv(f"{output_dir}/{category.lower().replace(' ', '_')}_distribution.csv")
    
    # Save multiple choice summaries
    pd.DataFrame({'Count': like_data}).to_csv(f"{output_dir}/liked_features_summary.csv")
    pd.DataFrame({'Count': improve_data}).to_csv(f"{output_dir}/improvement_areas_summary.csv")
    pd.DataFrame({'Count': feature_data}).to_csv(f"{output_dir}/desired_features_summary.csv")


def analyze_offline_functionality(df, output_dir):
    """
    Analyze and visualize offline functionality metrics
    """
    print("Analyzing offline functionality...")
    
    # Create mapping dictionaries
    helpfulness_mapping = {
        5: "Very helpful",
        4: "Somewhat helpful",
        3: "Not very helpful",
        2: "Not at all helpful",
        1: "Did not use"
    }
    
    accuracy_mapping = {
        5: "Mostly accurate",
        4: "Somewhat accurate",
        3: "Neither accurate nor inaccurate",
        2: "Somewhat inaccurate",
        1: "Mostly inaccurate"
    }
    
    importance_mapping = {
        4: "Very necessary - I use DurakGO for this",
        3: "Very important",
        2: "Somewhat important",
        1: "Not important"
    }
    
    # 1. Offline mode helpfulness
    plt.figure(figsize=(10, 6))
    helpfulness_counts = df['OFF_1'].value_counts().sort_index()
    helpfulness_labels = [helpfulness_mapping.get(i, f"Unknown ({i})") for i in helpfulness_counts.index]
    
    sns.barplot(x=helpfulness_labels, y=helpfulness_counts.values)
    plt.title('Helpfulness of Offline Mode')
    plt.xlabel('Helpfulness')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/offline_helpfulness.png", dpi=300)
    plt.close()
    
    # 2. Offline accuracy perception
    plt.figure(figsize=(10, 6))
    accuracy_counts = df['OFF_2'].value_counts().sort_index()
    accuracy_labels = [accuracy_mapping.get(i, f"Unknown ({i})") for i in accuracy_counts.index]
    
    sns.barplot(x=accuracy_labels, y=accuracy_counts.values)
    plt.title('Perceived Accuracy of Offline Predictions')
    plt.xlabel('Accuracy')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/offline_accuracy.png", dpi=300)
    plt.close()
    
    # 3. Importance of offline functionality
    plt.figure(figsize=(10, 6))
    importance_counts = df['OFF_3'].value_counts().sort_index()
    importance_labels = [importance_mapping.get(i, f"Unknown ({i})") for i in importance_counts.index]
    
    sns.barplot(x=importance_labels, y=importance_counts.values)
    plt.title('Importance of Offline Functionality')
    plt.xlabel('Importance')
    plt.ylabel('Count')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(f"{output_dir}/offline_importance.png", dpi=300)
    plt.close()
    
    # Relationship between offline functionality and overall satisfaction
    if 'OFF_2' in df.columns and 'SAT_1' in df.columns:
        plt.figure(figsize=(10, 6))
        
        # Group by offline accuracy perception and calculate mean app rating
        accuracy_satisfaction = df.groupby('OFF_2')['SAT_1'].mean().reset_index()
        accuracy_satisfaction['Accuracy'] = accuracy_satisfaction['OFF_2'].map(accuracy_mapping)
        
        sns.barplot(x='Accuracy', y='SAT_1', data=accuracy_satisfaction)
        plt.title('Relationship Between Perceived Offline Accuracy and App Satisfaction')
        plt.xlabel('Perceived Offline Accuracy')
        plt.ylabel('Average App Rating')
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.savefig(f"{output_dir}/offline_accuracy_vs_satisfaction.png", dpi=300)
        plt.close()
    
    # Save offline functionality metrics to CSV
    offline_summary = {
        'Offline Helpfulness': df['OFF_1'].map(helpfulness_mapping).value_counts(normalize=True) * 100,
        'Offline Accuracy': df['OFF_2'].map(accuracy_mapping).value_counts(normalize=True) * 100,
        'Offline Importance': df['OFF_3'].map(importance_mapping).value_counts(normalize=True) * 100
    }
    
    for category, distribution in offline_summary.items():
        distribution_df = pd.DataFrame(distribution)
        distribution_df.columns = ['Percentage']
        distribution_df.index.name = category
        distribution_df.to_csv(f"{output_dir}/{category.lower().replace(' ', '_')}_distribution.csv")


def create_cross_tabulations(df, output_dir):
    """
    Create cross-tabulations to explore relationships between key variables
    """
    print("Creating cross-tabulations...")
    
    # 1. App usage frequency vs. satisfaction - use a heatmap
    if 'USE_2' in df.columns and 'SAT_1' in df.columns:
        usage_satisfaction = pd.crosstab(df['USE_2'], df['SAT_1'], normalize='index') * 100
        usage_satisfaction.to_csv(f"{output_dir}/usage_frequency_vs_satisfaction.csv")
        
        # Create a visualization using heatmap
        plt.figure(figsize=(12, 8))
        
        usage_freq_mapping = {
            1: "Multiple times per day",
            2: "Once per day",
            3: "Several times per week",
            4: "Once per week or less"
        }
        
        rating_mapping = {
            1: "1 - Very Poor",
            2: "2 - Poor",
            3: "3 - Average",
            4: "4 - Good",
            5: "5 - Excellent"
        }
        
        # Rename the indices and columns
        usage_satisfaction.index = [usage_freq_mapping.get(i, str(i)) for i in usage_satisfaction.index]
        usage_satisfaction.columns = [rating_mapping.get(i, str(i)) for i in usage_satisfaction.columns]
        
        # Create the heatmap
        sns.heatmap(usage_satisfaction, annot=True, fmt='.1f', cmap='viridis',
                   cbar_kws={'label': 'Percentage (%)'})
        plt.title('App Usage Frequency vs. Satisfaction (%)')
        plt.xlabel('Satisfaction Rating')
        plt.ylabel('Usage Frequency')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/usage_frequency_vs_satisfaction_heatmap.png", dpi=300)
        plt.close()
        
        # Also create a grouped bar chart for mean satisfaction by usage frequency
        plt.figure(figsize=(10, 6))
        
        # Group by usage frequency and calculate mean app rating
        usage_satisfaction_mean = df.groupby('USE_2')['SAT_1'].mean().reset_index()
        usage_satisfaction_mean['Usage Frequency'] = usage_satisfaction_mean['USE_2'].map(usage_freq_mapping)
        
        # Create bar chart with better colors
        sns.barplot(x='Usage Frequency', y='SAT_1', data=usage_satisfaction_mean, 
                   palette='viridis')
        
        # Add mean value labels
        for i, v in enumerate(usage_satisfaction_mean['SAT_1']):
            plt.text(i, v + 0.05, f"{v:.2f}", ha='center')
        
        plt.title('App Usage Frequency vs. Average Satisfaction')
        plt.xlabel('Usage Frequency')
        plt.ylabel('Average Satisfaction Rating (1-5)')
        plt.ylim(0, 5.5)  # Set y limit to accommodate the text labels
        plt.tight_layout()
        plt.savefig(f"{output_dir}/usage_frequency_vs_avg_satisfaction.png", dpi=300)
        plt.close()
    
    # 2. Age group vs. satisfaction
    if 'DEM_5' in df.columns and 'SAT_1' in df.columns:
        age_satisfaction = pd.crosstab(df['DEM_5'], df['SAT_1'])
        age_satisfaction.to_csv(f"{output_dir}/age_group_vs_satisfaction.csv")
        
        # Create a visualization
        plt.figure(figsize=(10, 6))
        
        # Group by age and calculate mean app rating
        age_satisfaction_mean = df.groupby('DEM_5')['SAT_1'].mean().reset_index()
        
        age_mapping = {
            1: "Under 18",
            2: "18-24",
            3: "25-34",
            4: "35-44",
            5: "45-54",
            6: "55-64",
            7: "65-74",
            8: "75+"
        }
        
        age_satisfaction_mean['Age Group'] = age_satisfaction_mean['DEM_5'].map(age_mapping)
        
        sns.barplot(x='Age Group', y='SAT_1', data=age_satisfaction_mean)
        plt.title('Age Group vs. Average Satisfaction')
        plt.xlabel('Age Group')
        plt.ylabel('Average Satisfaction Rating')
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.savefig(f"{output_dir}/age_group_vs_avg_satisfaction.png", dpi=300)
        plt.close()
    
    # 3. Gender vs. satisfaction
    if 'DEM_6' in df.columns and 'SAT_1' in df.columns:
        gender_satisfaction = pd.crosstab(df['DEM_6'], df['SAT_1'])
        gender_satisfaction.to_csv(f"{output_dir}/gender_vs_satisfaction.csv")
        
        # Create a visualization
        plt.figure(figsize=(10, 6))
        
        # Group by gender and calculate mean app rating
        gender_satisfaction_mean = df.groupby('DEM_6')['SAT_1'].mean().reset_index()
        
        gender_mapping = {
            1: "Male",
            2: "Female",
            3: "Other",
            4: "Prefer not to say"
        }
        
        gender_satisfaction_mean['Gender'] = gender_satisfaction_mean['DEM_6'].map(gender_mapping)
        
        sns.barplot(x='Gender', y='SAT_1', data=gender_satisfaction_mean)
        plt.title('Gender vs. Average Satisfaction')
        plt.xlabel('Gender')
        plt.ylabel('Average Satisfaction Rating')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/gender_vs_avg_satisfaction.png", dpi=300)
        plt.close()
    
    # 4. Metro usage frequency vs. satisfaction with DurakGO
    if 'DEM_1' in df.columns and 'SAT_1' in df.columns:
        metro_satisfaction = pd.crosstab(df['DEM_1'], df['SAT_1'])
        metro_satisfaction.to_csv(f"{output_dir}/metro_usage_vs_satisfaction.csv")
        
        # Create a visualization
        plt.figure(figsize=(10, 6))
        
        # Group by metro usage and calculate mean app rating
        metro_satisfaction_mean = df.groupby('DEM_1')['SAT_1'].mean().reset_index()
        
        usage_mapping = {
            1: "Daily",
            2: "2-3 times per week",
            3: "Once a week",
            4: "2-3 times per month",
            5: "Once a month or less"
        }
        
        metro_satisfaction_mean['Metro Usage'] = metro_satisfaction_mean['DEM_1'].map(usage_mapping)
        
        sns.barplot(x='Metro Usage', y='SAT_1', data=metro_satisfaction_mean)
        plt.title('Metro Usage Frequency vs. Average App Satisfaction')
        plt.xlabel('Metro Usage Frequency')
        plt.ylabel('Average Satisfaction Rating')
        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.savefig(f"{output_dir}/metro_usage_vs_avg_satisfaction.png", dpi=300)
        plt.close()


def generate_summary_report(df, output_dir):
    """
    Generate a summary report of key metrics
    """
    print("Generating summary report...")
    
    # Calculate key metrics
    num_responses = len(df)
    avg_app_rating = df['SAT_1'].mean() if 'SAT_1' in df.columns else None
    avg_system_rating = df['IMP_3'].mean() if 'IMP_3' in df.columns else None
    
    # Wait time reduction
    wait_time_diff = []
    for _, row in df.iterrows():
        if pd.notna(row['WAIT_1']) and pd.notna(row['WAIT_2']):
            wait_time_diff.append(row['WAIT_1'] - row['WAIT_2'])
    
    avg_wait_reduction = np.mean(wait_time_diff) if wait_time_diff else None
    
    # Net Promoter Score
    if 'SAT_5' in df.columns:
        promoters = len(df[df['SAT_5'] >= 4])
        detractors = len(df[df['SAT_5'] <= 2])
        total_responses = len(df[pd.notna(df['SAT_5'])])
        
        nps = (promoters - detractors) / total_responses * 100
    else:
        nps = None
    
    # Usage impact
    if 'IMP_1' in df.columns:
        usage_increase = len(df[df['IMP_1'] >= 4]) / len(df[pd.notna(df['IMP_1'])]) * 100
    else:
        usage_increase = None
    
    # Create summary DataFrame
    summary_data = {
        'Metric': [
            'Number of Survey Responses',
            'Average App Rating (1-5)',
            'Average Transit System Rating (1-5)',
            'Average Perceived Wait Time Reduction',
            'Net Promoter Score (NPS)',
            'Percentage Reporting Increased Transit Usage'
        ],
        'Value': [
            num_responses,
            avg_app_rating,
            avg_system_rating,
            avg_wait_reduction,
            nps,
            usage_increase
        ]
    }
    
    summary_df = pd.DataFrame(summary_data)
    summary_df.to_csv(f"{output_dir}/summary_report.csv", index=False)
    
    # Create a text summary as well
    with open(f"{output_dir}/summary_report.txt", 'w') as f:
        f.write("=== DurakGO Survey Analysis Summary ===\n\n")
        f.write(f"Number of Survey Responses: {num_responses}\n")
        
        if avg_app_rating is not None:
            f.write(f"Average App Rating (1-5): {avg_app_rating:.2f}\n")
        
        if avg_system_rating is not None:
            f.write(f"Average Transit System Rating (1-5): {avg_system_rating:.2f}\n")
        
        if avg_wait_reduction is not None:
            f.write(f"Average Perceived Wait Time Reduction: {avg_wait_reduction:.2f} categories\n")
        
        if nps is not None:
            f.write(f"Net Promoter Score (NPS): {nps:.1f}\n")
        
        if usage_increase is not None:
            f.write(f"Percentage Reporting Increased Transit Usage: {usage_increase:.1f}%\n")


def main():
    """
    Main function to perform exploratory data analysis on DurakGO survey data
    """
    print("Starting DurakGO Survey Exploratory Data Analysis...")
    
    # Debug information
    import os
    cwd = os.getcwd()
    print(f"Current working directory: {cwd}")
    
    # Load data
    file_path = "Analysis/Survey_Data/final_data.csv"
    abs_path = os.path.abspath(file_path)
    print(f"Absolute path: {abs_path}")
    print(f"File exists: {os.path.exists(file_path)}")
    print(f"Absolute path exists: {os.path.exists(abs_path)}")
    
    df = load_survey_data(file_path)
    
    if df is None:
        print("Failed to load data. Exiting.")
        return
    
    # Create output directory
    output_dir = create_output_directory()
    
    # Prepare categorical data (expand multiple choice fields)
    df = prepare_categorical_data(df)
    
    # Perform various analyses
    describe_survey_metadata(df, output_dir)
    analyze_demographics(df, output_dir)
    analyze_usage_patterns(df, output_dir)
    analyze_wait_time_impact(df, output_dir)
    analyze_satisfaction_and_impact(df, output_dir)
    analyze_offline_functionality(df, output_dir)
    create_cross_tabulations(df, output_dir)
    
    # Generate summary report
    generate_summary_report(df, output_dir)
    
    print(f"Exploratory data analysis completed. Results saved to {output_dir}/")


if __name__ == "__main__":
    main()
