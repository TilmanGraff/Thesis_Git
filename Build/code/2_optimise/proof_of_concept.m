%% Read in global data and define parameters
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Code
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Ipopt-31

clear;


% Defines parameters
alpha = 0.7;
gamma = 0.946;
beta = 1.2446 * gamma;

sigma = 5;
a = 0.7; % production function parameter. Now in mobile labor it does matter, and I need to adjust productivities accordingly. Will do so below.
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...

N = 3;

adj = [0 1 0 1; 1 0 1 0; 0 1 0 1; 1 1 0 0];
x = [1 2 3 2];
y = [1 2 1 1];
pop = [100 100 100 1]';
%Zjn = [1 0 ; 0.1 0;  0 1];
Zjn = [1 0 0; 0 0.001 0; 0 0 1; 0, 1, 0];

I_stat = [0 1 0.00 0.001; 1 0 1 0; 0.00 1 0 0.001; 0.001 0 0.001 0];
delta_I = [0 1 1 1; 1 0 1 1; 1 1 0 1; 1 1 1 1];
delta_tau = [0 10 1 1; 10 0 10 1; 1 10 0 1; 1 1 1 1];




%% second attempt 
adj = [0 1 0; 1 0 1; 0 1 0];
x = [1 2 2.5];
y = [1 1.1 1.1];

N = 2;
pop = [1 10 2]';
Zjn = [100 0; 0 1; 10 0];

I_stat = [0 1 0; 1 0 0; 0 0 0];
delta_I = [1 1 0.5; 1 1 1; 1 0.5 1];
delta_tau = [1 1 1; 1 1 1; 1 1 1];



K = 1.1 .* sum(sum(delta_I .* I_stat));


param = init_parameters('Annealing', 'off', 'LaborMobility', 'on', 'a', a, 'sigma', sigma, ...
     'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'on', 'rho', rho, 'K', K, ...
     'CrossGoodCongestion', 'off');
[param,g]=create_graph(param,[],[],'Type','custom','Adjacency',adj,'X',x, 'Y',y);

            
param.Lj = pop; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
param.Zjn = Zjn;

g.delta_i = delta_I;
g.delta_tau = delta_tau;

param.tol_kappa = 1e-8
param.a = 1.1

[res_stat, flag, x0] = solve_allocation(param,g,I_stat, true);

res_anneal = annealing(param,g,I_stat,'Il',I_stat)


res_opt = optimal_network(param,g,I_stat,I_stat, [], x0)

dd
plot_graph(param, g, res_opt.Ijk)

res_opt.Ijk

[res_stat.uj res_opt.uj]
res_opt.uj ./ res_stat.uj 

