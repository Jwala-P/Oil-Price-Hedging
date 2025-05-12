# -*- coding: utf-8 -*-
"""
Created on Sun Mar 16 20:31:22 2025

@author: valan
"""

# Installation of required packages
# Run these commands in your terminal or command prompt before running the code
"""
#pip install numpy pandas matplotlib scikit-learn tensorflow
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import GRU, Dense, Dropout
from tensorflow.keras.callbacks import EarlyStopping
import os

# Set random seeds for reproducibility
np.random.seed(42)
import tensorflow as tf
tf.random.set_seed(42)

# Load your data
def load_data(filepath):
    df = pd.read_csv(filepath)
    # Convert the first column to datetime if it's a date
    df.iloc[:, 0] = pd.to_datetime(df.iloc[:, 0])
    df.set_index(df.columns[0], inplace=True)
    print(df.shape)
    print(f"Data loaded successfully. Shape: {df.shape}")
    print(f"Columns: {df.columns.tolist()}")
    return df

# Function to prepare data for GRU model with 80-20 split
def prepare_data(df, target_col='Close', sequence_length=60):
    # Ensure all data is numeric
    numeric_df = df.select_dtypes(include=[np.number])
    print(f"Using {numeric_df.shape[1]} numeric features")
    
    # Create target scaler (for inverting predictions later)
    target_scaler = MinMaxScaler(feature_range=(0, 1))
    target_scaler.fit(df[[target_col]].values)
    
    # Scale all features
    scaler = MinMaxScaler(feature_range=(0, 1))
    scaled_data = scaler.fit_transform(numeric_df)
    
    # Create sequences
    X, y = [], []
    for i in range(len(scaled_data) - sequence_length):
        X.append(scaled_data[i:i + sequence_length])
        # Get the target column index
        target_idx = list(numeric_df.columns).index(target_col)
        y.append(scaled_data[i + sequence_length, target_idx])
    
    X, y = np.array(X), np.array(y)
    print(f"Created {len(X)} sequences with shape {X.shape}")
    
    # Calculate the split point at 80%
    train_size = int(len(X) * 0.8)
    
    # Split data into train and test sets
    X_train, X_test = X[:train_size], X[train_size:]
    y_train, y_test = y[:train_size], y[train_size:]
    
    print(f"Train set: {X_train.shape}, Test set: {X_test.shape}")
    
    return X_train, X_test, y_train, y_test, scaler, target_scaler, numeric_df, train_size

# Function to build and train the GRU model
def build_gru_model(X_train, y_train, X_test, y_test, epochs=100, batch_size=32):
    # Get input dimensions
    n_timesteps, n_features = X_train.shape[1], X_train.shape[2]
    
    # Build model
    model = Sequential()
    model.add(GRU(units=50, return_sequences=True, input_shape=(n_timesteps, n_features)))
    model.add(Dropout(0.2))
    model.add(GRU(units=50, return_sequences=False))
    model.add(Dropout(0.2))
    model.add(Dense(units=1))
    
    # Compile model
    model.compile(optimizer='adam', loss='mean_squared_error')
    
    print(model.summary())
    
    # Set up early stopping
    early_stop = EarlyStopping(monitor='val_loss', patience=10, restore_best_weights=True)
    
    # Train model
    history = model.fit(
        X_train, y_train,
        epochs=epochs,
        batch_size=batch_size,
        validation_data=(X_test, y_test),
        callbacks=[early_stop],
        verbose=1
    )
    
    return model, history

# Function to make predictions
def make_predictions(model, X_test, target_scaler, y_test, numeric_df, target_col):
    # Make predictions
    print("Making predictions...")
    y_pred = model.predict(X_test)
    
    # Get original scale target values
    # First, create a container for the scaled values
    y_test_container = np.zeros((len(y_test), len(numeric_df.columns)))
    y_pred_container = np.zeros((len(y_pred), len(numeric_df.columns)))
    
    # Put the scaled values in the right column
    target_idx = list(numeric_df.columns).index(target_col)
    y_test_container[:, target_idx] = y_test
    y_pred_container[:, target_idx] = y_pred.flatten()
    
    # Transform back to original scale
    y_test_actual = target_scaler.inverse_transform(y_test_container)[:, target_idx]
    y_pred_actual = target_scaler.inverse_transform(y_pred_container)[:, target_idx]
    
    return y_test_actual, y_pred_actual

# Improved plotting function with years on x-axis
def plot_with_years(df, y_test_actual, y_pred_actual, target_col, sequence_length, train_size):
    """
    Creates a time series plot with years on x-axis, showing actual values for the entire dataset
    and predicted values for only the test portion (last 20%).
    """
    # Get the entire actual data
    actual_full = df[target_col].values
    
    # Create a dataframe with the full data
    result_df = pd.DataFrame(index=df.index)
    result_df['Actual'] = actual_full
    
    # Calculate the dates for the test data
    # We need to account for sequence_length and train_size
    test_start_idx = train_size + sequence_length
    test_dates = df.index[test_start_idx:]
    
    # Add predicted values only for the test portion
    result_df['Predicted'] = np.nan  # Initialize with NaN
    
    # Make sure we have enough test dates for our predictions
    if len(test_dates) >= len(y_pred_actual):
        result_df.loc[test_dates[:len(y_pred_actual)], 'Predicted'] = y_pred_actual
    else:
        print(f"Warning: Not enough test dates ({len(test_dates)}) for predictions ({len(y_pred_actual)})")
        # Use as many as we can
        result_df.loc[test_dates, 'Predicted'] = y_pred_actual[:len(test_dates)]
    
    # Create the plot
    plt.figure(figsize=(15, 8))
    
    # Plot full actual data
    plt.plot(result_df.index, result_df['Actual'], color='blue', label='Actual')
    
    # Plot predictions for test portion only
    plt.plot(result_df.index, result_df['Predicted'], color='red', label='Predicted')
    
    # Add vertical line at the train/test split
    split_date = df.index[test_start_idx]
    plt.axvline(x=split_date, color='green', linestyle='--', 
                label=f'Train-Test Split (80%)')
    
    # Format x-axis to show years
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    plt.gca().xaxis.set_major_locator(mdates.YearLocator())
    
    # Add labels and title
    plt.title(f'Oil Price Prediction - {target_col}', fontsize=16)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Price', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True, alpha=0.3)
    
    # Rotate x-axis labels for better readability
    plt.xticks(rotation=45)
    
    # Adjust layout
    plt.tight_layout()
    
    # Save the figure
    plt.savefig('oil_price_prediction_with_years.png', dpi=300)
    
    # Show the plot
    plt.show()
    
    return result_df

# Function to plot only test period predictions
def plot_test_predictions(df, y_test_actual, y_pred_actual, sequence_length, train_size, target_col='Close'):
    """
    Creates a plot showing only the test period with actual vs predicted values.
    
    Parameters:
    df (DataFrame): Original dataframe with datetime index
    y_test_actual (array): Actual values from test set
    y_pred_actual (array): Predicted values from model
    sequence_length (int): Number of timesteps used in each sequence
    train_size (int): Number of sequences in training set
    target_col (str): Name of target column being predicted
    
    Returns:
    DataFrame: DataFrame containing actual and predicted values
    """
    # Calculate the correct start index for test predictions
    test_start_idx = train_size + sequence_length
    
    # Get only the test period dates
    test_dates = df.index[test_start_idx:test_start_idx + len(y_pred_actual)]
    
    # Create a dataframe for the test period only
    test_df = pd.DataFrame(index=test_dates)
    test_df['Actual'] = y_test_actual[:len(test_dates)]
    test_df['Predicted'] = y_pred_actual[:len(test_dates)]
    
    # Create the plot
    plt.figure(figsize=(12, 7))
    
    # Plot data
    plt.plot(test_df.index, test_df['Actual'], 'b-', linewidth=2, label='Actual')
    plt.plot(test_df.index, test_df['Predicted'], 'r--', linewidth=2, label='Predicted')
    
    # Format x-axis based on the date range
    date_range = (test_df.index[-1] - test_df.index[0]).days
    
    if date_range > 365*2:  # More than 2 years
        plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
        plt.gca().xaxis.set_major_locator(mdates.YearLocator())
    elif date_range > 90:  # More than 3 months
        plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
        plt.gca().xaxis.set_major_locator(mdates.MonthLocator(interval=3))
    else:  # Less than 3 months
        plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%d %b'))
        plt.gca().xaxis.set_major_locator(mdates.WeekdayLocator(interval=2))
    
    # Add labels and title
    plt.title(f'Oil Price Prediction - Test Period ({target_col})', fontsize=16)
    plt.xlabel('Time Period', fontsize=12)
    plt.ylabel('Price', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True, alpha=0.3)
    
    # Calculate and display error metrics on the plot
    mse = np.mean((test_df['Actual'] - test_df['Predicted'])**2)
    rmse = np.sqrt(mse)
    mape = np.mean(np.abs((test_df['Actual'] - test_df['Predicted']) / test_df['Actual'])) * 100
    
    plt.figtext(0.15, 0.15, f'RMSE: {rmse:.2f}\nMAPE: {mape:.2f}%', 
                bbox=dict(facecolor='white', alpha=0.8), fontsize=10)
    
    # Rotate x-axis labels for better readability
    plt.xticks(rotation=45)
    
    # Adjust layout
    plt.tight_layout()
    
    # Save the figure
    plt.savefig('oil_price_test_predictions.png', dpi=300)
    
    # Show the plot
    plt.show()
    
    return test_df

# Function to evaluate model
def evaluate_model(y_test_actual, y_pred_actual):
    # Calculate metrics
    from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
    mse = mean_squared_error(y_test_actual, y_pred_actual)
    rmse = np.sqrt(mse)
    mae = mean_absolute_error(y_test_actual, y_pred_actual)
    r2 = r2_score(y_test_actual, y_pred_actual)
    
    print("\nModel Performance Metrics:")
    print(f'Mean Squared Error: {mse:.4f}')
    print(f'Root Mean Squared Error: {rmse:.4f}')
    print(f'Mean Absolute Error: {mae:.4f}')
    print(f'R² Score: {r2:.4f}')
    
    # Calculate percentage error
    mape = np.mean(np.abs((y_test_actual - y_pred_actual) / y_test_actual)) * 100
    print(f'Mean Absolute Percentage Error: {mape:.2f}%')

# Main function
def main(filepath, target_col='Close', sequence_length=60, epochs=100, batch_size=32):
    print(f"Starting oil price prediction with GRU model...")
    
    # Load data
    df = load_data(filepath)
    
    # Prepare data with 80-20 split
    X_train, X_test, y_train, y_test, scaler, target_scaler, numeric_df, train_size = prepare_data(
        df, target_col, sequence_length
    )
    
    # Build and train model
    model, history = build_gru_model(X_train, y_train, X_test, y_test, epochs, batch_size)
    
    # Make predictions
    y_test_actual, y_pred_actual = make_predictions(model, X_test, target_scaler, y_test, numeric_df, target_col)
    
    # Plot both visualizations
    # 1. Full dataset with train/test split
    result_df = plot_with_years(df, y_test_actual, y_pred_actual, target_col, sequence_length, train_size)
    
    # 2. Test period only with metrics
    test_df = plot_test_predictions(df, y_test_actual, y_pred_actual, sequence_length, train_size, target_col)
    
    # Evaluate model
    evaluate_model(y_test_actual, y_pred_actual)
    
    # Plot training history
    plt.figure(figsize=(10, 6))
    plt.plot(history.history['loss'], label='Training loss')
    plt.plot(history.history['val_loss'], label='Validation loss')
    plt.title('Model Loss')
    plt.xlabel('Epochs')
    plt.ylabel('Loss')
    plt.legend()
    plt.savefig('training_history.png')
    plt.show()
    
    return model, history, y_test_actual, y_pred_actual, result_df, test_df

# Example usage
if __name__ == "__main__":
    # Replace with your actual file path
    filepath = "C:/Users/valan/OneDrive/Desktop/Project/Round 2/Crude Oil Price Prediction/dec/R36_col_op.csv"
    target_col = "Close"  # Column to predict
    
    # Run the pipeline
    model, history, y_test_actual, y_pred_actual, result_df,test_df = main(
        filepath=filepath,
        target_col=target_col,
        sequence_length=60,  # Number of time steps to use as input
        epochs=100,
        batch_size=32
    )