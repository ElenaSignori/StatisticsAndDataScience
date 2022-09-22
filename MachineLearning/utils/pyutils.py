def round_pred(model, X):
    pred = model.predict(X).flatten().round()
    return pred

def rmse(y, pred):
    from statistics import mean
    rmse_score = mean((y - pred) ** 2) ** 0.5
    return rmse_score

def print_model_scores(model, X_train, X_valid, X_test, y_train, y_valid, y_test):
    pred_train = round_pred(model, X_train)
    pred_valid = round_pred(model, X_valid)
    pred_test = round_pred(model, X_test)

    rmse_train = rmse(y_train, pred_train)
    rmse_valid = rmse(y_valid, pred_valid)
    rmse_test = rmse(y_test, pred_test)

    print("Training RMSE: ", rmse_train)
    print("Validation RMSE: ", rmse_valid)
    print("Testing RMSE: ", rmse_test)