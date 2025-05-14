import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
from scipy import stats


def load_survey_data(file_path):
    """
    Load and prepare survey data for correlation analysis
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
    Create directory for correlation analysis outputs
    """
    output_dir = "Survey_Results/Analysis Results/Correlation_Analysis"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    return output_dir


def prepare_data_for_correlation(df):
    """
    Prepare data for correlation analysis by selecting relevant variables
    and cleaning/transforming as needed
    """
    # Create more intuitive column names for key variables
    variable_mapping = {
        'WAIT_1': 'Wait Time Before',
        'WAIT_2': 'Wait Time After',
        'WAIT_3': 'Wait Experience',
        'WAIT_4': 'Relaxation Effect',
        'IMP_1': 'Transit Usage Change',
        'IMP_2': 'Comparison to EGO Cep\'te',
        'IMP_3': 'System Rating',
        'SAT_1': 'App Rating',
        'SAT_5': 'Recommendation Likelihood',
        'OFF_1': 'Offline Helpfulness',
        'OFF_2': 'Offline Accuracy',
        'OFF_3': 'Offline Importance',
        'USE_1': 'Length of Time Using App',
        'USE_2': 'App Usage Frequency',
        'USE_5': 'Offline Usage',
        'DEM_1': 'Metro Usage Frequency',
        'DEM_2': 'Primary Transportation Mode',
        'DEM_5': 'Age Group',
        'DEM_6': 'Gender'
    }

    # Select variables for correlation analysis
    selected_vars = list(variable_mapping.keys())
    analysis_df = df[selected_vars].copy()

    # Rename columns for clarity
    analysis_df = analysis_df.rename(columns=variable_mapping)

    # Handle any missing values (use pairwise deletion in correlation calculations)
    print(f"Missing values before processing:\n{analysis_df.isnull().sum()}")

    return analysis_df


def calculate_correlations(df, method='spearman'):
    """
    Calculate correlation matrix using specified method
    """
    print(f"Calculating {method} correlations...")
    if method == 'spearman':
        corr_matrix = df.corr(method='spearman')
        title = "Spearman Rank Correlation Matrix"
    elif method == 'pearson':
        corr_matrix = df.corr(method='pearson')
        title = "Pearson Correlation Matrix"
    elif method == 'kendall':
        corr_matrix = df.corr(method='kendall')
        title = "Kendall's Tau Correlation Matrix"
    else:
        raise ValueError(f"Unsupported correlation method: {method}")

    return corr_matrix, title


def calculate_pvalues(df, method='spearman'):
    """
    Calculate p-values for correlation coefficients
    """
    print(f"Calculating p-values for {method} correlations...")

    # Initialize matrices
    corr_matrix = pd.DataFrame(np.zeros((df.shape[1], df.shape[1])),
                               index=df.columns, columns=df.columns)
    p_matrix = pd.DataFrame(np.zeros((df.shape[1], df.shape[1])),
                            index=df.columns, columns=df.columns)

    # Calculate correlation and p-value for each pair
    for i, col1 in enumerate(df.columns):
        for j, col2 in enumerate(df.columns):
            if method == 'spearman':
                corr, p = stats.spearmanr(df[col1], df[col2], nan_policy='omit')
            elif method == 'pearson':
                corr, p = stats.pearsonr(df[col1].dropna(), df[col2].dropna())
            elif method == 'kendall':
                corr, p = stats.kendalltau(df[col1], df[col2], nan_policy='omit')

            corr_matrix.iloc[i, j] = corr
            p_matrix.iloc[i, j] = p

    return corr_matrix, p_matrix


def visualize_correlation_matrix(corr_matrix, title, output_dir, filename):
    """
    Create and save a visualization of the correlation matrix
    """
    plt.figure(figsize=(16, 14))
    mask = np.triu(np.ones_like(corr_matrix, dtype=bool))
    cmap = sns.diverging_palette(230, 20, as_cmap=True)

    # Generate heatmap
    sns.heatmap(corr_matrix, mask=mask, cmap=cmap, vmax=1, vmin=-1, center=0,
                square=True, linewidths=.5, annot=True, fmt=".2f", annot_kws={"size": 8})

    plt.title(title, fontsize=16)
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    plt.savefig(f"{output_dir}/{filename}", dpi=300)
    plt.close()


def find_strongest_correlations(corr_matrix, p_matrix=None, threshold=0.3, alpha=0.05):
    """
    Identify strongest correlations above threshold and below significance level
    """
    # Create a list to store strong correlations
    strong_correlations = []

    # Iterate through the matrix (lower triangle only to avoid duplicates)
    for i in range(len(corr_matrix.columns)):
        for j in range(i):
            corr = corr_matrix.iloc[i, j]

            # Check if correlation exceeds threshold
            if abs(corr) >= threshold:
                entry = {
                    'Variable 1': corr_matrix.columns[i],
                    'Variable 2': corr_matrix.columns[j],
                    'Correlation': corr,
                    'Abs_Correlation': abs(corr)
                }

                # Add p-value if available
                if p_matrix is not None:
                    p_value = p_matrix.iloc[i, j]
                    entry['P_Value'] = p_value
                    entry['Significant'] = p_value <= alpha

                strong_correlations.append(entry)

    # Convert to DataFrame and sort
    strong_correlations_df = pd.DataFrame(strong_correlations)
    if len(strong_correlations_df) > 0:
        strong_correlations_df = strong_correlations_df.sort_values('Abs_Correlation', ascending=False)
        if p_matrix is not None:
            # Filter for significant correlations if p-values are available
            sig_correlations_df = strong_correlations_df[strong_correlations_df['Significant']]
            if len(sig_correlations_df) > 0:
                print(f"Found {len(sig_correlations_df)} significant correlations above threshold {threshold}")
                return sig_correlations_df

        print(f"Found {len(strong_correlations_df)} correlations above threshold {threshold}")
        return strong_correlations_df

    print("No strong correlations found")
    return pd.DataFrame()


def analyze_key_relationships(df, output_dir):
    """
    Analyze specific key relationships in more detail
    """
    # 1. Wait time reduction vs satisfaction
    if 'Wait Time Before' in df.columns and 'Wait Time After' in df.columns and 'App Rating' in df.columns:
        df['Wait Time Difference'] = df['Wait Time Before'] - df['Wait Time After']

        plt.figure(figsize=(10, 6))

        # Create scatter plot with regression line
        sns.regplot(x='Wait Time Difference', y='App Rating', data=df, scatter_kws={'alpha': 0.5})

        plt.title('Relationship Between Wait Time Reduction and App Satisfaction')
        plt.xlabel('Wait Time Reduction (minutes)')
        plt.ylabel('App Rating (1-5)')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/wait_time_vs_satisfaction.png", dpi=300)
        plt.close()

        # Calculate correlation and p-value
        corr, p = stats.spearmanr(df['Wait Time Difference'], df['App Rating'], nan_policy='omit')
        print(f"Correlation between wait time reduction and satisfaction: {corr:.3f} (p = {p:.4f})")

    # 2. Offline accuracy vs satisfaction
    if 'Offline Accuracy' in df.columns and 'App Rating' in df.columns:
        plt.figure(figsize=(10, 6))

        # Create box plot
        sns.boxplot(x='Offline Accuracy', y='App Rating', data=df)

        plt.title('Relationship Between Perceived Offline Accuracy and App Satisfaction')
        plt.xlabel('Perceived Offline Accuracy (1-5)')
        plt.ylabel('App Rating (1-5)')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/offline_accuracy_vs_satisfaction.png", dpi=300)
        plt.close()

    # 3. Wait experience vs transit usage change
    if 'Wait Experience' in df.columns and 'Transit Usage Change' in df.columns:
        plt.figure(figsize=(10, 6))

        # Create box plot
        sns.boxplot(x='Wait Experience', y='Transit Usage Change', data=df)

        plt.title('Relationship Between Wait Experience and Transit Usage Change')
        plt.xlabel('Wait Experience (1-5, 5=Much Shorter)')
        plt.ylabel('Transit Usage Change (1-5, 5=Much More Frequent)')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/wait_experience_vs_usage_change.png", dpi=300)
        plt.close()


def main():
    """
    Main function to perform correlation analysis
    """
    print("Starting DurakGO Survey Correlation Analysis...")
    
    # Debug information
    import os
    cwd = os.getcwd()
    print(f"Current working directory: {cwd}")
    
    # Try multiple path options
    # Option 1: When run from the Analysis/Survey Analysis directory
    file_path_1 = "../Survey_Data/final_data.csv"
    # Option 2: When run from the root directory
    file_path_2 = "Analysis/Survey_Data/final_data.csv"
    
    # Check which path exists and use it
    if os.path.exists(file_path_1):
        file_path = file_path_1
        print(f"Using path option 1: {file_path}")
    elif os.path.exists(file_path_2):
        file_path = file_path_2
        print(f"Using path option 2: {file_path}")
    else:
        # Try absolute path as a last resort
        cwd = os.getcwd()
        if "Paper Analysis" in cwd:
            file_path = os.path.join(cwd, "Analysis", "Survey_Data", "final_data.csv")
        else:
            parent_dir = os.path.dirname(cwd)
            file_path = os.path.join(parent_dir, "Survey_Data", "final_data.csv")
        print(f"Using fallback path: {file_path}")
    
    print(f"Final path used: {file_path}")
    print(f"File exists: {os.path.exists(file_path)}")
    
    # Load the data
    df = load_survey_data(file_path)
    
    if df is None:
        print("Failed to load data. Exiting.")
        return

    # Create output directory
    output_dir = create_output_directory()
    print(f"Created output directory: {output_dir}")
    
    # Prepare data for correlation analysis
    analysis_df = prepare_data_for_correlation(df)
    
    # Calculate Spearman correlations
    corr_matrix, title = calculate_correlations(analysis_df, method='spearman')
    
    # Calculate p-values
    _, p_matrix = calculate_pvalues(analysis_df, method='spearman')
    
    # Visualize correlation matrix
    visualize_correlation_matrix(corr_matrix, title, output_dir, "spearman_correlation_matrix.png")
    
    # Find strongest correlations
    strong_correlations = find_strongest_correlations(corr_matrix, p_matrix, threshold=0.3)
    
    # Save strongest correlations to CSV
    if len(strong_correlations) > 0:
        strong_correlations = strong_correlations.drop('Abs_Correlation', axis=1)
        strong_correlations.to_csv(f"{output_dir}/significant_correlations.csv", index=False)
    
    # Analyze key relationships
    analyze_key_relationships(analysis_df, output_dir)
    
    print(f"Correlation analysis completed. Results saved to {output_dir}.")


if __name__ == "__main__":
    main()