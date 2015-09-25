function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);

for iter = 1:num_iters
    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %

% X = 97 * 2 , theta = 2 * 1
Prediction = X*theta;

% sumerr = 97 * 1
err = (Prediction - y);

for i = 1: length(theta)
#   theta(i) = theta(i) - (alpha/m)*(transpose(err)*X(:,i));
    theta(i) = theta(i) - (alpha/m)*sum((err).*X(:,i));
end

    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end

subplot(1,2,1);
plot(J_history,"ro",'MarkerSize',1);
end
