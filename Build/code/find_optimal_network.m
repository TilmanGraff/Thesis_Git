
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

% Imports matrices

adj = csvread("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/Nigeria.csv", 1, 1);
speed = csvread("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/speed/Nigeria.csv", 1, 1);
dist = csvread("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/dist/Nigeria.csv", 1, 1);

% Imports underlying characteristics



% Defines initial population

population = ones(n,1);

% Defines initial housing

housing = ones(n,1);

% Defines initial production

production = (productivity) .* (population);

% p = plot(g, 'XData', x_coords, 'YData', y_coords, 'NodeLabel', {});


% Defines links
%links = initial_infrastructure;
%links(half_point, half_point+1) = 10;


%%
% Second try, go directly via reduced lagrangian

% Analytical FOCs:

% Define U(c_j,h_j)
rho = 2;
alpha = 0.9;
beta = 0.8;
gamma = 0.7;
delta_tau = 0.6*ones(n);
delta_I = 0.5*ones(n);
weights = ones(n, 1);
kappa = gamma*(1+beta)^(-(1+beta)/beta);
K = sum(sum(initial_infrastructure));

syms U(c,h)

U = @(c,h) (((c.^alpha).*(h.^(1-alpha))).^(1-rho))./(1-rho);
U_c = matlabFunction(diff(U,c));

U_c_inv = matlabFunction(finverse(U_c, c)); % Note that c now is the inverted argument

% Define FOCs as function of prices -- OPTIMAL NETWORK

opt_links_raw = @(P) adj .* ((kappa*(delta_I.*(delta_tau.^(1./beta))).^(-1)) .* ...
    max(zeros(n), -1 + (transpose((P)*transpose(P.^-1))) ).^((1+beta)/beta) .* P).^(beta/(beta-gamma));
opt_links = @(P) opt_links_raw(P) .* (K ./ sum(sum(delta_I .* opt_links_raw(P))));

Q = @(P) (1/(1+beta).*(opt_links(P).^gamma)./(delta_tau) .* max(zeros(n), -1 + (transpose((P)* ...
    transpose(P.^-1))) )).^(1/beta);
c = @(P) U_c_inv(P ./ weights, housing);
outflows = @(P) nansum(Q(P) + delta_tau .* (Q(P).^(1+beta) .* opt_links(P) .^(-1*gamma)),2);
inflows = @(P) transpose(sum(Q(P),1));

% Objective Function

Lagrangian = @(P) sum(weights .* population .* U(c(P),housing)) - sum(P .* ...
    (c(P) + outflows(P) - production  - inflows(P)));

% Optimisation
P0 = transpose(0.1:(0.8/(n-1)):0.9);
options_con = optimoptions('fmincon');
options_con.MaxFunctionEvaluations = 30000;
options_con.MaxIterations = 10000;
A = -1 .* eye(n);
b = zeros(n, 1);
P = fmincon(Lagrangian, P0, A, b, [], [], [],[], [], options_con);

eventual_infrastructure = opt_links(P);
opt_tradeflows = Q(P);


% STATIC INFRASTRUCTURE

opt_links = @(P) initial_infrastructure;
Q = @(P) (1/(1+beta).*(opt_links(P).^gamma)./(delta_tau) .* max(zeros(n), -1 + (transpose((P)* ...
    transpose(P.^-1))) )).^(1/beta);
c = @(P) U_c_inv(P ./ weights, housing);
outflows = @(P) nansum(Q(P) + ...
    delta_tau .* (Q(P).^(1+beta) .* opt_links(P) .^(-1*gamma)),2);
inflows = @(P) transpose(sum(Q(P),1));

% Objective Function

Lagrangian = @(P) sum(weights .* population .* U(c(P),housing)) - sum(P.* ...
    (c(P) + outflows(P) - production  - inflows(P)));

P0 = transpose(0.1:(0.8/(n-1)):0.9);
options_con = optimoptions('fmincon');
options_con.MaxFunctionEvaluations = 30000; options_con.MaxIterations = 10000;...
    A = -1 .* eye(n); b = zeros(n, 1);
P_stat = fmincon(Lagrangian, P0, A, b, [], [], [],[], [], options_con);

static_tradeflows = Q(P_stat);

welfare_gain = sum(weights .* population .* U(c(P),housing)) / ...
    sum(weights .* population .* U(c(P_stat),housing)) - 1;

%%
% Plot results
%
% tradeflows = digraph(round(Q(P), 10));
% LWidths = 20*tradeflows.Edges.Weight/max(tradeflows.Edges.Weight);
% MSize = 20*production / max(production) + 10;
% p = plot(tradeflows, 'XData', x_coords, 'YData', y_coords, 'NodeLabel', c(P), ...
% 'LineWidth',LWidths, 'ArrowSize', 5);
% p.MarkerSize = MSize;
% p.NodeCData = P;
% title('Predicted trade flows')
% colorbar


g1 = graph(initial_infrastructure);
g2 = digraph(static_tradeflows);
g3= graph(symMat(eventual_infrastructure));
g4 = digraph(opt_tradeflows);


g1_weights = 10*g1.Edges.Weight/max(g3.Edges.Weight) + 2;
g2_weights = 10*g2.Edges.Weight/max(g4.Edges.Weight);
g3_weights = 10*g3.Edges.Weight/max(g3.Edges.Weight);
g4_weights = 10*g4.Edges.Weight/max(g4.Edges.Weight);

MSize1 = 20*production / max(production) + 1;
MSize2 = 20*c(P_stat) / max(c(P_stat)) + 1;
MSize3 = MSize1;
MSize4 = 20*c(P) / max(c(P)) + 1;

figure
subplot(2,2,1)
plot(g1, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g1_weights,...
    'MarkerSize', MSize1,  'NodeLabel', {})
title('Initial Infrastructure')

subplot(2,2,2)
plot(g2, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g2_weights,...
    'MarkerSize', MSize2,  'NodeLabel', {}, 'NodeCData', P_stat)
colorbar
title('Initial Tradeflows')

subplot(2,2,3)
plot(g3, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g3_weights,...
    'MarkerSize', MSize3,  'NodeLabel', {})
title('Optimal Infrastructure')

subplot(2,2,4)
plot(g4, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g4_weights,...
    'MarkerSize', MSize4,  'NodeLabel', {}, 'NodeCData', P)
colorbar
title('Optimal Tradeflows')

welfare_gain

%%
%scatter(c(P), P)
