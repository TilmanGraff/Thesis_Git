
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

%% Read in global data and define parameters

clear;

country_table = readtable("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/country_names.csv");
country_names = table2array(country_table);

centroids = readtable("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);


% Defines parameters

rho = 2;
alpha = 0.9;
beta = 1.245;
gamma = 0.5*beta;
sigma = 4;
a = 0.7;
% kappa = gamma*(1+beta)^(-(1+beta)/beta);

% Defines FOCs (together with more complex functions Q and I)
c = @(P) ((1/alpha) * (sum(P.^(1-sigma), 2)).^(1/(1-sigma))).^(1/(alpha-1)); % per capita total consumption in each location
C = @(P, population) (P./(sum(P.^(1-sigma), 2)).^(1/(1-sigma))).^(-sigma) .* (population.*c(P));
L = @(P, productivity, population) (((P .* productivity) .^(1/(1-alpha))) ./ sum((P .* productivity) .^(1/(1-alpha)), 2)) .* population;

outflows = @(P, I, delta_tau, delta_I, adj) reshape(sum(Q(P, I, adj, delta_tau, beta, gamma) + delta_tau .* ((Q(P, I, adj, delta_tau, beta, gamma)).^(1+beta)) .* I.^(-gamma), 2, 'omitnan'), [size(P,1), size(P,2)]);
inflows = @(P, I, delta_tau, delta_I, adj) reshape(sum(Q(P, I, adj, delta_tau, beta, gamma), 1), [size(P,1), size(P,2)]);

% Lagrange  = @(P) sum(weights .* population .* (c(P).^(alpha)), 1) ...
%     - sum(sum(P .* (C(P, population) + outflows(P, I, delta_tau, delta_I, adj) - inflows(P, I, delta_tau, delta_I, adj) ...
%     - productivity .* (population.^(a)))));

%% For each country

% for countryID = 1:length(country_names)
for countryID = 3
    countryname = (country_names(countryID));
    if exist(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"))
        
        % Split centroids by country
        case_centroids = centroids(centroids.country == countryname,:);

        % Read in characteristics
        population = cellfun(@str2double, case_centroids.pop);

        % Read in the relevant matrices
        adj = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/delta_I/delta_I_", (countryname), ".csv"), 1, 0);
        delta_tau = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/delta_tau/delta_tau_", (countryname), ".csv"), 1, 0);
        I = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"), 1, 0);
    
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);
        
        % Don't really need them now, as they are all ones. But might come
        % in handy later?
        weights = ones(J, 1);
        
        %% Optimisation
        
        % defines static problem with current infrastructure
        Lagrange_I_static  = @(P) deal((sum(weights .* population .* (c(P).^(alpha)), 1) ...
        - sum(sum(P .* (C(P, population) + outflows(P, I, delta_tau, delta_I, adj) - inflows(P, I, delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)))))), ...
        -reshape(C(P, population) + outflows(P, I, delta_tau, delta_I, adj) - inflows(P, I, delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)), [J*N 1]));
        
        % defines full problem with optimal infrastructure
        Lagrange_I_opt  = @(P) deal((sum(weights .* population .* (c(P).^(alpha)), 1) ...
        - sum(sum(P .* (C(P, population) + outflows(P, I_opt(P, I, adj, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) - inflows(P, I_opt(P, I, adj, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)))))), ...
        -reshape(C(P, population) + outflows(P, I_opt(P, I, adj, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) - inflows(P, I_opt(P, I, adj, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) ...
        - productivity .* (population.^(a)), [J*N 1]));
        
        % Define options for different solving algorithms (On 10. Dec, I
        % prefer trustregion)
        options_interiorpoint = optimoptions('fmincon','SpecifyObjectiveGradient',true, 'MaxIterations', 5000, 'MaxFunctionEvaluations', 30000);
        options_trustregion = optimoptions('fmincon','SpecifyObjectiveGradient',true, 'Algorithm', 'trust-region-reflective');

        % Set initial conditions and solver boundaries
        P0 = rand(J, N).*100;
        A = [];
        b = [];
        Aeq = [];
        beq = [];
        lb = zeros(J, N);
        ub = [];
        nonlcon = [];
        
        % Optimise static problem
        [P_stat,Lagrangevalue_stat,Exitflag_stat] = fmincon(Lagrange_I_static, P0,A,b,Aeq,beq,lb,ub, nonlcon, options_trustregion);
        
        % Optimise full problem
        [P_opt,Lagrangevalue_opt,Exitflag_opt] = fmincon(Lagrange_I_opt, P0,A,b,Aeq,beq,lb,ub, nonlcon, options_trustregion);
        
        
        %% Obtain descriptive statistics
        
        optimal_infrastructure = I_opt(P_opt, I, adj, delta_tau, delta_I, beta, gamma);
        
        raw_tradeflows_stat = sum(Q(P_stat, I, adj, delta_tau, beta, gamma), 3);
        raw_tradeflows_opt = sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma),3);
        
        total_tradeflows_stat = sum(sum(sum(Q(P_stat, I, adj, delta_tau, beta, gamma))));
        total_tradeflows_opt = sum(sum(sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma))));
        
        consumption_stat = (c(P_stat).^alpha);
        consumption_opt = (c(P_opt).^alpha);
        
        welfare_stat = sum(sum((c(P_stat).^alpha).*population));
        welfare_opt = sum(sum((c(P_opt).^alpha).*population));

        price_index_stat = (sum(P_stat.^(1-sigma), 2)).^(1/(1-sigma));
        price_index_opt = (sum(P_opt.^(1-sigma), 2)).^(1/(1-sigma));        
        
        
        %% Plot graphs
        
        graph_stat = digraph(raw_tradeflows_stat);
        edgeweights_stat = 5*(graph_stat.Edges.Weight / (max(graph_stat.Edges.Weight)));
        msize_stat = 25*((c(P_stat).^alpha).*population) / max((c(P_stat).^alpha).*population)+0.1;

        graph_opt = digraph(raw_tradeflows_opt);
        edgeweights_opt = 5*(graph_opt.Edges.Weight / (max(graph_opt.Edges.Weight)));
        msize_opt = 25*((c(P_opt).^alpha).*population) / max((c(P_opt).^alpha).*population)+0.1;

        
        f = figure;
        p = uipanel('Parent',f,'BorderType','none', 'BackgroundColor','white'); 
        p.Title = countryname; 
        p.TitlePosition = 'centertop'; 
        p.FontSize = 12;
        p.FontWeight = 'bold';
        
        subplot(2,1,1, 'Parent', p)
        plot_1 = plot(graph_stat, 'XData', (cellfun(@str2double, case_centroids.x)), 'YData', (cellfun(@str2double, case_centroids.y)), 'Linewidth', edgeweights_stat, 'MarkerSize', msize_stat, 'NodeCData', price_index_stat);
        colorbar;
        title('Current Infrastructure');
        
        subplot(2,1,2,'Parent', p)
        plot_2 = plot(graph_opt, 'XData', (cellfun(@str2double, case_centroids.x)), 'YData', (cellfun(@str2double, case_centroids.y)), 'Linewidth', edgeweights_opt, 'MarkerSize', msize_opt, 'NodeCData', price_index_opt);
        colorbar;
        title('Optimal Infrastructure');
        
        f.PaperPositionMode = 'auto';
        print('-fillpage',strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Matlab_graphs/optimised_network/", (countryname), "_graph"),'-dpdf')
        
        
        %% Export data
        
        % Optimal Network
        csvwrite(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Optimised_Networks/", (countryname), ".csv"), optimal_infrastructure);
        
        % Raw tradeflows and Optimal Tradeflows
        csvwrite(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Tradeflows/Initial_Flows_", (countryname), ".csv"), raw_tradeflows_stat);
        csvwrite(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Tradeflows/Optimal_Flows_", (countryname), ".csv"), raw_tradeflows_opt);

        % Consumption Distribution
        csvwrite(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Consumption/Initial_c_", (countryname), ".csv"), consumption_stat);
        csvwrite(strcat("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Consumption/Optimal_c_", (countryname), ".csv"), consumption_opt);

        
        
    end
    
    
    
    %clear adj delta_I delta_tau I population productivity
end






%%


% Lagrange_I_opt  = @(P) (sum(weights .* population .* (c(P).^(alpha)), 1) ...
%         - sum(sum(P .* (C(P, population) + outflows(P, I_opt(P, I, adj, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) - inflows(P, I_opt(P, I, adj, delta_tau, delta_I, beta, gamma), delta_tau, delta_I, adj) ...
%         - productivity .* (population.^(a))))));
% 
%           P0 = rand(J, N);
%         A = [];
%         b = [];
%         Aeq = [];
%         beq = [];
%         lb = zeros(J, N);
%         ub = [];
%         nonlcon = [];
%      [x,fval,exitflag,output,lambda,grad,hessian] = fmincon(Lagrange_I_opt, P0,A,b,Aeq,beq,lb,ub, nonlcon);

% Second try, go directly via reduced lagrangian

% % Analytical FOCs:
% 
% syms U(c,h)
% 
% U = @(c,h) (((c.^alpha).*(h.^(1-alpha))).^(1-rho))./(1-rho);
% U_c = matlabFunction(diff(U,c));
% 
% U_c_inv = matlabFunction(finverse(U_c, c)); % Note that c now is the inverted argument
% 
% % Define FOCs as function of prices -- OPTIMAL NETWORK
% 
% opt_links_raw = @(P) adj .* ((kappa*(delta_I.*(delta_tau.^(1./beta))).^(-1)) .* ...
%     max(zeros(n), -1 + (transpose((P)*transpose(P.^-1))) ).^((1+beta)/beta) .* P).^(beta/(beta-gamma));
% opt_links = @(P) opt_links_raw(P) .* (K ./ sum(sum(delta_I .* opt_links_raw(P))));
% 
% Q = @(P) (1/(1+beta).*(opt_links(P).^gamma)./(delta_tau) .* max(zeros(n), -1 + (transpose((P)* ...
%     transpose(P.^-1))) )).^(1/beta);
% c = @(P) U_c_inv(P ./ weights, housing);
% outflows = @(P) nansum(Q(P) + delta_tau .* (Q(P).^(1+beta) .* opt_links(P) .^(-1*gamma)),2);
% inflows = @(P) transpose(sum(Q(P),1));
% 
% % Objective Function
% 
% Lagrangian = @(P) sum(weights .* population .* U(c(P),housing)) - sum(P .* ...
%     (c(P) + outflows(P) - production  - inflows(P)));
% 
% % Optimisation
% P0 = transpose(0.1:(0.8/(n-1)):0.9);
% options_con = optimoptions('fmincon');
% options_con.MaxFunctionEvaluations = 30000;
% options_con.MaxIterations = 10000;
% A = -1 .* eye(n);
% b = zeros(n, 1);
% P = fmincon(Lagrangian, P0, A, b, [], [], [],[], [], options_con);
% 
% eventual_infrastructure = opt_links(P);
% opt_tradeflows = Q(P);
% 
% 
% % STATIC INFRASTRUCTURE
% 
% opt_links = @(P) initial_infrastructure;
% Q = @(P) (1/(1+beta).*(opt_links(P).^gamma)./(delta_tau) .* max(zeros(n), -1 + (transpose((P)* ...
%     transpose(P.^-1))) )).^(1/beta);
% c = @(P) U_c_inv(P ./ weights, housing);
% outflows = @(P) nansum(Q(P) + ...
%     delta_tau .* (Q(P).^(1+beta) .* opt_links(P) .^(-1*gamma)),2);
% inflows = @(P) transpose(sum(Q(P),1));
% 
% % Objective Function
% 
% Lagrangian = @(P) sum(weights .* population .* U(c(P),housing)) - sum(P.* ...
%     (c(P) + outflows(P) - production  - inflows(P)));
% 
% P0 = transpose(0.1:(0.8/(n-1)):0.9);
% options_con = optimoptions('fmincon');
% options_con.MaxFunctionEvaluations = 30000; options_con.MaxIterations = 10000;...
%     A = -1 .* eye(n); b = zeros(n, 1);
% P_stat = fmincon(Lagrangian, P0, A, b, [], [], [],[], [], options_con);
% 
% static_tradeflows = Q(P_stat);
% 
% welfare_gain = sum(weights .* population .* U(c(P),housing)) / ...
%     sum(weights .* population .* U(c(P_stat),housing)) - 1;
% 
% %%
% % Plot results
% %
% % tradeflows = digraph(round(Q(P), 10)); LWidths =
% % 20*tradeflows.Edges.Weight/max(tradeflows.Edges.Weight); MSize =
% % 20*production / max(production) + 10; p = plot(tradeflows, 'XData',
% % x_coords, 'YData', y_coords, 'NodeLabel', c(P), ... 'LineWidth',LWidths,
% % 'ArrowSize', 5); p.MarkerSize = MSize; p.NodeCData = P; title('Predicted
% % trade flows') colorbar
% 
% 
% g1 = graph(initial_infrastructure);
% g2 = digraph(static_tradeflows);
% g3= graph(symMat(eventual_infrastructure));
% g4 = digraph(opt_tradeflows);
% 
% 
% g1_weights = 10*g1.Edges.Weight/max(g3.Edges.Weight) + 2;
% g2_weights = 10*g2.Edges.Weight/max(g4.Edges.Weight);
% g3_weights = 10*g3.Edges.Weight/max(g3.Edges.Weight);
% g4_weights = 10*g4.Edges.Weight/max(g4.Edges.Weight);
% 
% MSize1 = 20*production / max(production) + 1;
% MSize2 = 20*c(P_stat) / max(c(P_stat)) + 1;
% MSize3 = MSize1;
% MSize4 = 20*c(P) / max(c(P)) + 1;
% 
% figure
% subplot(2,2,1)
% plot(g1, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g1_weights,...
%     'MarkerSize', MSize1,  'NodeLabel', {})
% title('Initial Infrastructure')
% 
% subplot(2,2,2)
% plot(g2, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g2_weights,...
%     'MarkerSize', MSize2,  'NodeLabel', {}, 'NodeCData', P_stat)
% colorbar
% title('Initial Tradeflows')
% 
% subplot(2,2,3)
% plot(g3, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g3_weights,...
%     'MarkerSize', MSize3,  'NodeLabel', {})
% title('Optimal Infrastructure')
% 
% subplot(2,2,4)
% plot(g4, 'XData', x_coords, 'YData', y_coords, 'LineWidth', g4_weights,...
%     'MarkerSize', MSize4,  'NodeLabel', {}, 'NodeCData', P)
% colorbar
% title('Optimal Tradeflows')
% 
% welfare_gain
% 
% %%
% %scatter(c(P), P)
