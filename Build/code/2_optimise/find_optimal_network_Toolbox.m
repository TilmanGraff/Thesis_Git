
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

%% Read in global data and define parameters
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Code
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Ipopt-31

clear;

country_table = readtable("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/country_names.csv");
country_names = table2array(country_table);

centroids = readtable("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);

% Defines parameters
alpha = 0.7;
gamma = 0.946;
beta = 1.2446 * gamma;

sigma = 5;
a = 0.7; % production function parameter. Now in mobile labor it does matter, and I need to adjust productivities accordingly. Will do so below.
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...


%% For each country
cs = strings(1);
pops = nan(1);
facts = nan(1);
nums = nan(1);

 for countryID = 1:length(country_names)
    
     countryname = (country_names(countryID))
     if exist(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv")) % && ~exist(strcat("//Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"))

        % Split centroids by country
        case_centroids = readtable(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
     %if num_locations > 300 && num_locations < 350
        if countryname == "Senegal"
        % Read in characteristics
        population = case_centroids.pop;


        % Read in the relevant matrices
        adj = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/adj/adj_", (countryname), ".csv"), 1, 0);
        abr = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/abr/abr_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/delta_I/delta_I_", (countryname), ".csv"), 1, 0);
        delta_tau = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/delta_tau/delta_tau_withcomp_fp_", (countryname), ".csv"), 1, 0);
        I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/productivities/productivities_", (countryname), ".csv"), 1, 0);

        
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);

        % Only optimise over the domestic grid cells
        weights = 1-case_centroids.abroad;
        
        K = 1.0;
        
        %% Initialise geography
        
        param = init_parameters('Annealing', 'off', 'LaborMobility', 'off', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'on', 'rho', rho, 'K', K, 'CrossGoodCongestion', 'off');
        
        [param,g]=create_graph(param,[],[],'X',case_centroids.x,'Y',case_centroids.y,'Type','custom','Adjacency',adj);
        
        
        param.Lj = population + (population == 0) .* 10^-7; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = productivity;
        %param.omegaj = weights;
        
        param.Hj = population .* (1-alpha); % I normalise this because the general utility function has a (h_j/(1-alpha))^(1-alpha) thing with it
        param.tol_kappa = 1.0e-7;

        
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;
       
        %param.tol_kappa = 1.0e-4;

        %% Optimisation
        
        min_mask = abr .* I + (adj - abr) .* 4;        
        max_mask = abr .* I + (adj - abr) .* 120;
        
        
        % Static
        strcat("Started P_stat on ", datestr(datetime('now')))
        %res_stat = optimal_network(param,g,I,I,I);
        res_stat = solve_allocation(param,g,I, false);
        %annrea = annealing(param,g,res.Ijk,'Il',min_mask,'Iu',max_mask);



        param.K = 1.1;
        min_mask = I;
        
        % Optimalq
        strcat("Started P_opt on ", datestr(datetime('now')))
        res_opt = optimal_network(param,g,I,min_mask,max_mask,false);
      

        ff
     
        %% Obtain descriptive statistics

        optimal_infrastructure = res_opt.Ijk;
%         raw_tradeflows_stat = sum(Q(P_stat, I, adj, delta_tau, beta, gamma), 3);
%         raw_tradeflows_opt = sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma),3);
% 
%         total_tradeflows_stat = sum(sum(sum(Q(P_stat, I, adj, delta_tau, beta, gamma))));
%         total_tradeflows_opt = sum(sum(sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma))));

        util_stat = res_stat.uj;
        util_opt = res_opt.uj;


        consumption_stat = res_stat.Cj;
        consumption_opt = res_opt.Cj;

        price_index_stat = res_stat.PCj;
        price_index_opt = res_opt.PCj;



        %% Export data

        % Optimal Network
        %csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Optimised_Networks/", (countryname), ".csv"), optimal_infrastructure);

        % Raw tradeflows and Optimal Tradeflows
        % csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Tradeflows/Initial_Flows_", (countryname), ".csv"), raw_tradeflows_stat);
        % csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Tradeflows/Optimal_Flows_", (countryname), ".csv"), raw_tradeflows_opt);


        % Location Characteristics
        % writetable(array2table([case_centroids.ID case_centroids.x case_centroids.y case_centroids.abroad population price_index_stat price_index_opt util_stat util_opt ...
        %   consumption_stat consumption_opt], ...
        %   'VariableNames', {'ID', 'x', 'y', 'abroad', 'pop', 'P_stat', 'P_opt', 'util_stat', 'util_opt', 'c_stat', 'c_opt'}), strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Network_outcomes/", (countryname), "_outcomes.csv"));

       end
    end
 end
