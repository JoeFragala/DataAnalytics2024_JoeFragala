"""
NAME: Joe Fragala
DATE: 4/20/2024
CLASS: CSCI 4210-02
ASSIGNMENT: Assignment 6 - Project (Land Temps vs Vegetation Index)

DESCRIPTION: Your term projects should fall within the scope of a data 
analytics problem of the type you have worked with in class/ labs, or know of 
yourself – the bigger the data the better. This means that the work must go 
beyond just making lots of figures. You should develop the project to indicate 
you are thinking of and exploring the relationships and distributions within 
your data. Start with a hypothesis, think of a way to model and use the 
hypothesis, find or collect the necessary data, and do both preliminary 
analysis, detailed modeling and summary (interpretation).
"""
from pyhdf.SD import SD, SDC
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns  # For nicer scatter plots
import random
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import mean_squared_error, r2_score


# Set the style of the seaborn plots
sns.set_context("talk", font_scale=1.1)
sns.set_style("whitegrid")


def load_dataset(hdf_file, dataset_name):
    """Utility function to load a specific dataset from an HDF file."""
    dataset = hdf_file.select(dataset_name)
    data = dataset[:]
    # Replace fill values with NaN for proper visualization
    attrs = dataset.attributes(full=1)
    fill_value = attrs["_FillValue"]
    
    # Use the fill value to replace with NaNs
    data = data.astype(float)
    data[data == fill_value[0]] = np.nan
    return data


def plot_data(data, title, cmap, units, filename):
    """Utility function to create a plot for a dataset."""
    # Mask fill values (if they are negative or otherwise known)
    data = np.ma.masked_less(data, 0)  # Assuming negative values are fill values
    
    # Use a larger figure size to make the graph larger
    fig, ax = plt.subplots(figsize=(10, 6))  # Adjust the figsize as needed
    fig.set_dpi(300)  # Increase DPI for better resolution
    cax = ax.imshow(data, cmap=cmap, interpolation='nearest', aspect='auto')
    # Adjust the colorbar size
    cbar = fig.colorbar(cax, orientation='vertical', fraction=0.025, pad=0.02)
    cbar.set_label(units, weight='bold', fontsize='small')
    cbar.ax.tick_params(labelsize='small')  # Make colorbar ticks larger
    plt.title(title, weight='bold', fontsize='x-large')
    plt.xlabel('Longitude Index', weight='bold', fontsize='large')
    plt.ylabel('Latitude Index', weight='bold', fontsize='large')
    plt.tight_layout()  # Adjust the layout
    plt.savefig(filename, dpi=300, bbox_inches='tight', pad_inches=0.1)  # Save as a high-resolution PNG file
    plt.show()


def plot_scatter(x, y, xlabel, ylabel, title, filename):
    """Utility function to create a scatter plot for two datasets."""
    # Remove NaN values for scatter plotting
    mask = ~np.isnan(x) & ~np.isnan(y)
    x, y = x[mask], y[mask]
    
    plt.figure(figsize=(8, 6))
    sns.scatterplot(x=x, y=y, edgecolor='w', s=20)
    plt.title(title, weight='bold', fontsize='x-large')
    plt.xlabel(xlabel, weight='bold', fontsize='large')
    plt.ylabel(ylabel, weight='bold', fontsize='large')
    plt.grid(True, which='both', linestyle='--', linewidth=0.5)  # Add grid
    plt.tight_layout()  # Adjust the layout
    plt.savefig(filename, dpi=300)  # Save as a high-resolution PNG file
    plt.show()

    # Calculate and print the correlation coefficient
    corr_coef = np.corrcoef(x, y)[0, 1]
    print(f"Correlation coefficient between {xlabel} and {ylabel}: {corr_coef:.3f}")


def random_sampling_plot_scatter(x, y, xlabel, ylabel, title, filename, sample_size=5000):
    """Utility function to create a scatter plot for two datasets."""
    # Remove NaN values for scatter plotting
    mask = ~np.isnan(x) & ~np.isnan(y)
    x, y = x[mask], y[mask]
    
    # Implement random sampling if the data is large
    if len(x) > sample_size:
        random_indices = random.sample(range(len(x)), sample_size)
        x = x[random_indices]
        y = y[random_indices]
    
    plt.figure(figsize=(8, 6))
    sns.scatterplot(x=x, y=y, edgecolor='w', s=20)
    plt.title(title, weight='bold', fontsize='x-large')
    plt.xlabel(xlabel, weight='bold', fontsize='large')
    plt.ylabel(ylabel, weight='bold', fontsize='large')
    plt.grid(True, which='both', linestyle='--', linewidth=0.5)  # Add grid
    plt.tight_layout()  # Adjust the layout
    plt.savefig(filename, dpi=300)  # Save as a high-resolution PNG file
    plt.show()

    # Calculate and print the correlation coefficient
    corr_coef = np.corrcoef(x, y)[0, 1]
    print(f"Correlation coefficient between {xlabel} and {ylabel} with Random Sampling: {corr_coef:.3f}")


def fit_linear_regression(X, y):
    """
    Fits a linear regression model to the data.
    """
    # Split the data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Initialize the model
    linear_model = LinearRegression()

    # Fit the model to the training data
    linear_model.fit(X_train, y_train)

    # Predict on the testing set
    y_pred = linear_model.predict(X_test)

    # Calculate performance metrics
    mse = mean_squared_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)

    print(f"Linear Regression MSE: {mse}")
    print(f"Linear Regression R2: {r2}")

    return linear_model, X_test, y_test, y_pred


def fit_decision_tree(X, y):
    """
    Fits a decision tree regressor to the data.
    """
    # Split the data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Initialize the model
    tree_model = DecisionTreeRegressor(random_state=42)

    # Fit the model to the training data
    tree_model.fit(X_train, y_train)

    # Predict on the testing set
    y_pred = tree_model.predict(X_test)

    # Calculate performance metrics
    mse = mean_squared_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)

    print(f"Decision Tree Regressor MSE: {mse}")
    print(f"Decision Tree Regressor R2: {r2}")

    return tree_model, X_test, y_test, y_pred


def plot_model_results(X_test, y_test, y_pred, title, xlabel, ylabel, filename):
    """
    Utility function to create a plot to compare the actual and predicted values from a model.
    """
    plt.figure(figsize=(10, 6))
    plt.scatter(X_test, y_test, color='blue', label='Actual', alpha=0.5)
    plt.scatter(X_test, y_pred, color='red', label='Predicted', alpha=0.5)
    
    # Plotting the perfect prediction line
    min_val = min(min(y_test), min(y_pred))
    max_val = max(max(y_test), max(y_pred))
    plt.plot([min_val, max_val], [min_val, max_val], color='green', linestyle='--', label='Perfect Prediction')
    
    plt.title(title, weight='bold', fontsize='x-large')
    plt.xlabel(xlabel, weight='bold', fontsize='large')
    plt.ylabel(ylabel, weight='bold', fontsize='large')
    plt.legend()
    plt.grid(True, which='both', linestyle='--', linewidth=0.5)
    plt.tight_layout()
    plt.savefig(filename, dpi=300)
    plt.show()


def sample_data(X, y, sample_size=5000, random_state=42):
    """Randomly sample data for modeling."""
    assert len(X) == len(y), "Features and target must have the same length"
    
    # Ensure the data is in the correct format
    X = np.array(X).reshape(-1, 1)
    y = np.array(y)
    
    # Sample the data
    if len(X) > sample_size:
        np.random.seed(random_state)
        indices = np.random.choice(len(X), sample_size, replace=False)
        X_sampled = X[indices]
        y_sampled = y[indices]
    else:
        X_sampled = X
        y_sampled = y
    
    return X_sampled, y_sampled


def fit_model_with_sampling(X, y, model_type='linear', random_state=42):
    """Fit a model with sampled data."""
    # Sample the data
    X_sampled, y_sampled = sample_data(X, y, sample_size=5000, random_state=random_state)
    
    # Split the data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(X_sampled, y_sampled, test_size=0.2, random_state=random_state)
    
    # Select the model type
    if model_type == 'linear':
        model = LinearRegression()
    elif model_type == 'tree':
        model = DecisionTreeRegressor(random_state=random_state)
    else:
        raise ValueError("Unsupported model type")
    
    # Fit the model
    model.fit(X_train, y_train)
    
    # Predict on the testing set
    y_pred = model.predict(X_test)
    
    # Performance metrics
    mse = mean_squared_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)
    
    print(f"{model_type.capitalize()} Model MSE: {mse:.4f}")
    print(f"{model_type.capitalize()} Model R2: {r2:.4f}")
    
    return model, X_test, y_test, y_pred



if __name__ == "__main__":
    # File paths
    file_path_TEMP = r'C:\Users\fragaj3\Dropbox\Spring2024\CSCI_4600_01_Data_Analytics\Project\Assignment6\Data\MODIS_Temp\MOD11C3.A2024061.061.2024094071904.hdf'
    file_path_VEG = r'C:\Users\fragaj3\Dropbox\Spring2024\CSCI_4600_01_Data_Analytics\Project\Assignment6\Data\MODIS_Veg\MOD13C2.A2024061.061.2024099222643.hdf'

    try:
        # Load temperature data
        TEMP_file = SD(file_path_TEMP, SDC.READ)
        LST_Day_CMG = load_dataset(TEMP_file, 'LST_Day_CMG')
        LST_Night_CMG = load_dataset(TEMP_file, 'LST_Night_CMG')

        # Load vegetation data
        VEG_file = SD(file_path_VEG, SDC.READ)
        NDVI = load_dataset(VEG_file, 'CMG 0.05 Deg Monthly NDVI')
        EVI = load_dataset(VEG_file, 'CMG 0.05 Deg Monthly EVI')

    finally:
        TEMP_file.end()
        VEG_file.end()

    # Plot the data using the utility function
    print("Modeling Data:")
    plot_data(LST_Day_CMG, 'Daytime Land Surface Temperature (LST)', cmap=plt.cm.hot, units='K', filename='Daytime_LST.png')
    plot_data(NDVI, 'Normalized Difference Vegetation Index (NDVI)', cmap=plt.cm.viridis, units='Index Value', filename='NDVI.png')
    plot_data(LST_Night_CMG, 'Nighttime Land Surface Temperature (LST)', cmap=plt.cm.cool, units='K', filename='Nighttime_LST.png')
    plot_data(EVI, 'Enhanced Vegetation Index (EVI)', cmap=plt.cm.viridis, units='Index Value', filename='EVI.png')
    print("Plots Printed")
    # Add the comparison code after loading the datasets
    # Ensure both datasets cover the same shape and mask out NaNs before comparison
    if LST_Day_CMG.shape != NDVI.shape:
        print("Error: Dataset shapes do not match for comparison!")
    else:
        print("\n\nModeling Scatterplots:")
        plot_scatter(
            LST_Day_CMG.flatten(), 
            NDVI.flatten(), 
            'Daytime LST (K)', 
            'NDVI (Index Value)', 
            'Scatter Plot of Daytime LST vs NDVI',
            'Daytime_LST_vs_NDVI_Scatter.png'
        )
        plot_scatter(
            LST_Night_CMG.flatten(), 
            NDVI.flatten(), 
            'Nighttime LST (K)', 
            'NDVI (Index Value)', 
            'Scatter Plot of Nighttime LST vs NDVI',
            'Nighttime_LST_vs_NDVI_Scatter.png'
        )
        plot_scatter(
            LST_Day_CMG.flatten(), 
            EVI.flatten(), 
            'Daytime LST (K)', 
            'EVI (Index Value)', 
            'Scatter Plot of Daytime LST vs EVI',
            'Daytime_LST_vs_EVI_Scatter.png'
        )
        plot_scatter(
            LST_Night_CMG.flatten(), 
            EVI.flatten(), 
            'Nighttime LST (K)', 
            'EVI (Index Value)', 
            'Scatter Plot of Nighttime LST vs EVI',
            'Nighttime_LST_vs_EVI_Scatter.png'
        )
        print("\n\nModeling Random Sampling Scatterplots:")
        random_sampling_plot_scatter(
            LST_Day_CMG.flatten(), 
            NDVI.flatten(), 
            'Daytime LST (K)', 
            'NDVI (Index Value)', 
            'Random Sampling Scatter Plot of Daytime LST vs NDVI',
            'RANDOM_SAMPLING_Daytime_LST_vs_NDVI_Scatter.png'
        )
        random_sampling_plot_scatter(
            LST_Night_CMG.flatten(), 
            NDVI.flatten(), 
            'Nighttime LST (K)', 
            'NDVI (Index Value)', 
            'Random Sampling Scatter Plot of Nighttime LST vs NDVI',
            'RANDOM_SAMPLING_Nighttime_LST_vs_NDVI_Scatter.png'
        )
        random_sampling_plot_scatter(
            LST_Day_CMG.flatten(), 
            EVI.flatten(), 
            'Daytime LST (K)', 
            'EVI (Index Value)', 
            'Random Sampling Scatter Plot of Daytime LST vs EVI',
            'RANDOM_SAMPLING_Daytime_LST_vs_EVI_Scatter.png'
        )
        random_sampling_plot_scatter(
            LST_Night_CMG.flatten(), 
            EVI.flatten(), 
            'Nighttime LST (K)', 
            'EVI (Index Value)', 
            'Random Sampling Scatter Plot of Nighttime LST vs EVI',
            'RANDOM_SAMPLING_Nighttime_LST_vs_EVI_Scatter.png'
        )
    
    # Flatten daytime and nighttime LST data
    LST_Day_CMG_flat = LST_Day_CMG.flatten()
    LST_Night_CMG_flat = LST_Night_CMG.flatten()
    
    # Flatten NDVI and EVI data
    NDVI_flat = NDVI.flatten()
    EVI_flat = EVI.flatten()
    
    # Create validity masks for daytime and nighttime data
    valid_mask_day = ~np.isnan(LST_Day_CMG_flat) & ~np.isnan(NDVI_flat) & ~np.isnan(EVI_flat)
    valid_mask_night = ~np.isnan(LST_Night_CMG_flat) & ~np.isnan(NDVI_flat) & ~np.isnan(EVI_flat)
    
    # Check for NaN values after applying the masks
    assert np.isnan(LST_Day_CMG_flat[valid_mask_day]).sum() == 0, "NaN values found in Day LST after masking"
    assert np.isnan(LST_Night_CMG_flat[valid_mask_night]).sum() == 0, "NaN values found in Night LST after masking"
    assert np.isnan(NDVI_flat[valid_mask_day]).sum() == 0, "NaN values found in NDVI after masking"
    assert np.isnan(EVI_flat[valid_mask_day]).sum() == 0, "NaN values found in EVI after masking"
    
    # Apply validity masks to flattened data for modeling
    LST_Day_CMG_valid = LST_Day_CMG_flat[valid_mask_day].reshape(-1, 1)
    LST_Night_CMG_valid = LST_Night_CMG_flat[valid_mask_night].reshape(-1, 1)
    NDVI_valid_day = NDVI_flat[valid_mask_day]
    NDVI_valid_night = NDVI_flat[valid_mask_night]  # This should be the same as NDVI_valid_day if NDVI does not change from day to night
    EVI_valid_day = EVI_flat[valid_mask_day]
    EVI_valid_night = EVI_flat[valid_mask_night]  # Same as above for EVI
    
    # Now assert that the lengths are equal
    assert len(LST_Day_CMG_valid) == len(NDVI_valid_day) == len(EVI_valid_day), "Mismatch in day data lengths after masking."
    assert len(LST_Night_CMG_valid) == len(NDVI_valid_night) == len(EVI_valid_night), "Mismatch in night data lengths after masking."


    # Modeling with NDVI
    print("\n\nModeling Linear Regression and Decision Tree Daytime LST vs NDVI:")
    lin_model_ndvi, X_test_lin_ndvi, y_test_lin_ndvi, y_pred_lin_ndvi = fit_linear_regression(LST_Day_CMG_valid, NDVI_valid_day)
    dtree_model_ndvi, X_test_tree_ndvi, y_test_tree_ndvi, y_pred_tree_ndvi = fit_decision_tree(LST_Day_CMG_valid, NDVI_valid_day)

    # For Daytime linear regression NDVI data
    plot_model_results(
        X_test_lin_ndvi, y_test_lin_ndvi, y_pred_lin_ndvi, 
        'Daytime Linear Regression NDVI', 'Daytime LST (K)', 'NDVI (Index Value)', 
        'daytime_linear_regression_ndvi_results.png'
    )
    
    # For Daytime decision tree NDVI data
    plot_model_results(
        X_test_tree_ndvi, y_test_tree_ndvi, y_pred_tree_ndvi, 
        'Daytime Decision Tree NDVI', 'Daytime LST (K)', 'NDVI (Index Value)', 
        'daytime_decision_tree_ndvi_results.png'
    )
    
    # Modeling with EVI
    print("\n\nModeling Linear Regression and Decision Tree Daytime LST vs EVI:")
    lin_model_evi, X_test_lin_evi, y_test_lin_evi, y_pred_lin_evi = fit_linear_regression(LST_Day_CMG_valid, EVI_valid_day)
    dtree_model_evi, X_test_tree_evi, y_test_tree_evi, y_pred_tree_evi = fit_decision_tree(LST_Day_CMG_valid, EVI_valid_day)

    # For Daytime linear regression EVI data
    plot_model_results(
        X_test_lin_evi, y_test_lin_evi, y_pred_lin_evi,
        'Daytime Linear Regression EVI', 'Daytime LST (K)', 'EVI (Index Value)',
        'daytime_linear_regression_evi_results.png'
    )
    
    # For Daytime decision tree EVI data
    plot_model_results(
        X_test_tree_evi, y_test_tree_evi, y_pred_tree_evi,
        'Daytime Decision Tree EVI', 'Daytime LST (K)', 'EVI (Index Value)',
        'daytime_decision_tree_evi_results.png'
    )

    # Modeling with NDVI
    print("\n\nModeling Linear Regression and Decision Tree Nighttime LST vs NDVI:")
    lin_model_ndvi_night, X_test_night_lin_ndvi, y_test_night_lin_ndvi, y_pred_night_lin_ndvi = fit_linear_regression(LST_Night_CMG_valid, NDVI_valid_night)
    dtree_model_ndvi_night, X_test_night_tree_ndvi, y_test_night_tree_ndvi, y_pred_night_tree_ndvi = fit_decision_tree(LST_Night_CMG_valid, NDVI_valid_night)
    
    # For Nighttime linear regression NDVI data
    plot_model_results(
        X_test_night_lin_ndvi, y_test_night_lin_ndvi, y_pred_night_lin_ndvi, 
        'Nighttime Linear Regression NDVI', 'Nighttime LST (K)', 'NDVI (Index Value)', 
        'nighttime_linear_regression_ndvi_results.png'
    )
    
    # For Nighttime decision tree NDVI data
    plot_model_results(
        X_test_night_tree_ndvi, y_test_night_tree_ndvi, y_pred_night_tree_ndvi, 
        'Nighttime Decision Tree NDVI', 'Nighttime LST (K)', 'NDVI (Index Value)', 
        'nighttime_decision_tree_ndvi_results.png'
    )
    
    # Modeling with EVI
    print("\n\nModeling Linear Regression and Decision Tree Nighttime LST vs EVI:")
    lin_model_evi_night, X_test_night_lin_evi, y_test_night_lin_evi, y_pred_night_lin_evi = fit_linear_regression(LST_Night_CMG_valid, EVI_valid_night)
    dtree_model_evi_night, X_test_night_tree_evi, y_test_night_tree_evi, y_pred_night_tree_evi = fit_decision_tree(LST_Night_CMG_valid, EVI_valid_night)
    
    # For Nighttime linear regression EVI data
    plot_model_results(
        X_test_night_lin_evi, y_test_night_lin_evi, y_pred_night_lin_evi, 
        'Nighttime Linear Regression EVI', 'Nighttime LST (K)', 'EVI (Index Value)', 
        'nighttime_linear_regression_evi_results.png'
    )
    
    # For Nighttime decision tree EVI data
    plot_model_results(
        X_test_night_tree_evi, y_test_night_tree_evi, y_pred_night_tree_evi, 
        'Nighttime Decision Tree EVI', 'Nighttime LST (K)', 'EVI (Index Value)', 
        'nighttime_decision_tree_evi_results.png'
    )