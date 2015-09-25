function [theta, J_history] = gradientDescentLogit(x, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 3);

for iter = 1:num_iters
    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %

% Theta = Theta - 1/m * alpha* sum (h(x) - y) * x(i)
% x = 49 * 4 , theta = 4 * 1
Prediction = 1./(1 + exp(-(x*theta)));

% sumerr = 49 * 1
err = (Prediction - y);

for i = 1: length(theta)
#   theta(i) = theta(i) - (alpha/m)*(transpose(err)*x(:,i));
    theta(i) = theta(i) - (alpha/m)*sum((err).*x(:,i));
end

% ============================================================

    % Save the cost J in every iteration    
    J_history(iter,1) = computeCostLogit(x, y, theta);
    J_history(iter,2) = theta(1);
    J_history(iter,3) = theta(2);

end

plot(J_history(:,1),'ro','MarkerSize',1)

% Surface plot
#figure;
#surf(J_history(:,2),J_history(:,3),J_history(:,1)*J_history(:,1)')
#xlabel('\theta_0'); ylabel('\theta_1');

% Contour plot
#figure;
% Plot J_vals as 15 contours spaced logarithmically between 0.01 and 100
#contour(J_history(:,2),J_history(:,3),J_history(:,1)*J_history(:,1)', #logspace(-2, 3, 20))
#xlabel('\theta_0'); ylabel('\theta_1');

#subplot(1,2,1);
#plot(J_history,"ro",'MarkerSize',1);

end
