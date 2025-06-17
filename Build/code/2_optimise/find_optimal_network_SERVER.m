
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

%% Read in global data and define parameters
addpath /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/code/2_optimise/OptimalTransportNetworkToolbox/Code
addpath /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/code/2_optimise/OptimalTransportNetworkToolbox/Ipopt-3


% run("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/code/2_optimise/find_optimal_network_SERVER.m")

clear;

country_table = readtable("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/country_names.csv");
country_names = table2array(country_table);

centroids = readtable("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);
centroids.country = categorical(centroids.country);

% define outpath
foldername = strcat(datestr(now,"yyyy-mm-dd_HHMMSS"), "_imm_nocomp")
%foldername = "2024-02-17_175358_imm_fp"
outpath = strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/output/", foldername);

mkdir(outpath);
mkdir(strcat(outpath, "/Optimised_Networks/"))
mkdir(strcat(outpath, "/taus/"))
mkdir(strcat(outpath, "/Network_outcomes/"))


% Defines parameters

alpha = 0.7;
gamma = 0.946;
beta = 1.2446 * gamma;
sigma = 5;
a = 0.7; % production function parameter, it doesnt matter because production is fixed anyway. Just have to make sure that its the same as in the define_productivity_and_rownames.R file
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...

%% For each country

parpool(20);
 parfor countryID = 1:length(country_names)
 %for countryID = 1:length(country_names)
    
     countryname = (country_names(countryID))
     if exist(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/productivities/productivities_", (countryname), ".csv"), "file")  && ~exist(strcat(outpath, "/Network_outcomes/", (countryname), "_outcomes.csv"), "file")

        % Split centroids by country
        case_centroids = readtable(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
        if num_locations > 2 %&& num_locations < 600
        %if num_locations == 1071

        % Read in characteristics
        population = case_centroids.pop;


        % Read in the relevant matrices
        adj = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/adj/adj_", (countryname), ".csv"), 1, 0);
        abr = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/abr/abr_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/delta_I/delta_I_", (countryname), ".csv"), 1, 0);

        %delta_tau = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/delta_tau/delta_tau_withcomp_", (countryname), ".csv"), 1, 0);
        %delta_tau = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/delta_tau/delta_tau_", (countryname), ".csv"), 1, 0);
        %delta_tau = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/delta_tau/delta_tau_fp_", (countryname), ".csv"), 1, 0);
        delta_tau = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/delta_tau/delta_tau_fp_", (countryname), ".csv"), 1, 0);

        I = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat("/n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/temp/productivities/productivities_", (countryname), ".csv"), 1, 0);

        
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);

        % Only optimise over the domestic grid cells
        weights = 1-case_centroids.abroad;
        weights(weights == 0) = 1.0e-7;

        K = 1.0; 
        
        
        %% Initialise geography
        
        param = init_parameters('Annealing', 'off', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'off', 'rho', rho, 'K', K);

        [param,g]=create_graph(param,[],[],'X',case_centroids.x,'Y',case_centroids.y,'Type','custom','Adjacency',adj,'X',case_centroids.x,'Y',case_centroids.y);
        
        
        param.Lj = population + (population == 0) .* 10^-1; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = productivity;
        param.omegaj = weights;
        
        param.Hj = population .* (1-alpha); % I normalise this because the general utility function has a (h_j/(1-alpha))^(1-alpha) thing with it
        
        param.tol_kappa = 1.0e-5;
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;
       


        %% Optimisation
        
        min_mask = abr .* I + (adj - abr) .* 4;        
       % min_mask = I; % this is for the 10perc exercise
        max_mask = abr .* I + (adj - abr) .* 120;


        
        % Static
        strcat(countryname)
        strcat(countryname, ": Started P_stat on ", datestr(datetime('now')))
        
       
        
        %res_stat = optimal_network(param,g,I,I,I);
        [res_stat,flag,x0] = solve_allocation(param,g,I);
        %annrea = annealing(param,g,res.Ijk,'Il',min_mask,'Iu',max_mask);
        

        
        % Optimal
        strcat(countryname, ": Started P_opt on ", datestr(datetime('now')))
        
      
        
        res_opt = optimal_network(param,g,I,min_mask,max_mask, false, x0);
        


        param.K = 1.1;
        min_mask = I;

        strcat(countryname, ": Started 10perc exercise on ", datestr(datetime('now')))
        res_opt_10p = optimal_network(param,g,I,min_mask,max_mask, false, x0);
     
     
        %% Obtain descriptive statistics

      
        optimal_infrastructure = res_opt.Ijk;
        optimal_infrastructure_10p = res_opt_10p.Ijk;

         raw_tradeflows_stat = res_stat.Qjkn;
         raw_tradeflows_opt = res_opt.Qjkn;

        tau_stat = delta_tau .* raw_tradeflows_stat.^beta ./ (I.^gamma);
        tau_opt = delta_tau .* raw_tradeflows_opt.^beta ./ (optimal_infrastructure.^gamma);
 
        tau_2D_stat = sum(tau_stat .* raw_tradeflows_stat, 3) ./ raw_tradeflows_stat;
        tau_2D_opt = sum(tau_opt .* raw_tradeflows_opt, 3) ./ raw_tradeflows_opt;

%          total_tradeflows_stat = sum(sum(sum(Q(P_stat, I, adj, delta_tau, beta, gamma))));
%          total_tradeflows_opt = sum(sum(sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma))));

        util_stat = res_stat.uj;
        util_opt = res_opt.uj;
        util_opt_10p = res_opt_10p.uj;


        consumption_stat = res_stat.Cj;
        consumption_opt = res_opt.Cj;
        consumption_opt_10p = res_opt_10p.Cj;
        
        price_index_stat = res_stat.PCj;
        price_index_opt = res_opt.PCj;
        price_index_opt_10p = res_opt_10p.PCj;

      
        %% Export data

        % Optimal Network
        csvwrite(strcat(outpath, "/Optimised_Networks/", (countryname), ".csv"), optimal_infrastructure);
        csvwrite(strcat(outpath, "/Optimised_Networks/", (countryname), "_10p.csv"), optimal_infrastructure_10p);

        % Raw tradeflows and Optimal Tradeflows
        csvwrite(strcat(outpath, "/taus/tau_stat_", (countryname), ".csv"), tau_2D_stat);
        csvwrite(strcat(outpath, "/taus/tau_opt_", (countryname), ".csv"), tau_2D_opt);

        pop_opt = res_opt.Lj .* sum(population);
        pop_opt_10p = res_opt_10p.Lj .* sum(population);


        % Location Characteristics
        writetable(array2table([case_centroids.ID case_centroids.x case_centroids.y case_centroids.abroad price_index_stat price_index_opt price_index_opt_10p ...
            util_stat util_opt util_opt_10p ...
          consumption_stat consumption_opt consumption_opt_10p param.Hj], ...
          'VariableNames', {'ID', 'x', 'y', 'abroad', 'P_stat', 'P_opt', 'P_opt_10p', 'util_stat', 'util_opt', 'util_opt_10p', 'c_stat', 'c_opt', 'c_opt_10p', 'amenities'}), strcat(outpath, "/Network_outcomes/", (countryname), "_outcomes.csv"));

       end
    end
 end

