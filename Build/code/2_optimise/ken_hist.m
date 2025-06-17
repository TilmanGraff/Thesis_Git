
% This file takes adj, dist, and speed matrices and calculates current tradeflows, optiomal networks, and welfare gains

%% Read in global data and define parameters
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Code
addpath /Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/code/2_optimise/OptimalTransportNetworkToolbox/Ipopt-31.nosync/

clear;

case_centroids = readtable("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/" + ...
    "output/ken_hist/centroids.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);

allpop = readtable("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/" + ...
    "output/ken_hist/hist_data/allpop.csv", 'TreatAsEmpty',{'.','NA'}, 'EmptyValue', 0);

% Defines parameters
alpha = 0.7;
gamma = 0.946;
beta = 1.2446 * gamma;

sigma = 5;
a = 1; % production function parameter. Now in mobile labor it does matter, and I need to adjust productivities accordingly. Will do so below.
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...


%% For each year
for year = 1962:2011
        
        if exist(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/" + ...
                "hist_data/prod_", string(year), ".csv"))
        
        population = allpop.(strcat('p', string(year)));


        % Read in the relevant matrices
        adj = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/adj.csv"), 1, 0);
        delta_I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/" + ...
                "hist_data/delta_i_", string(year), ".csv"), 1, 0);
        delta_tau = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/delta_tau.csv"), 1, 0);
        I = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/" + ...
                "hist_data/I_", string(year), ".csv"), 1, 0);
        productivity = csvread(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/" + ...
                "hist_data/prod_", string(year), ".csv"), 1, 0);

        
        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);
        
        K = 1.0;
        
        %% Initialise geography
        
        param = init_parameters('Annealing', 'off', 'LaborMobility', 'off', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'off', 'rho', rho, 'K', K, 'CrossGoodCongestion', 'off');
        
        [param,g]=create_graph(param,[],[],'X',case_centroids.centroidx,'Y',case_centroids.centroidy,'Type','custom','Adjacency',adj);
        
        
        param.Lj = population; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = productivity;
        %param.omegaj = weights;
        
        param.Hj = population .* (1-alpha); % I normalise this because the general utility function has a (h_j/(1-alpha))^(1-alpha) thing with it
        param.tol_kappa = 1.0e-7;

        
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;
       
        %param.tol_kappa = 1.0e-4;

        %% Optimisation
        
        min_mask = adj .* 4.0;        
        max_mask = adj .* 120;
        
        
        % Static
        strcat("Started P_stat on ", datestr(datetime('now')))
        %res_stat = optimal_network(param,g,I,I,I);
        [res_stat,flag, x0] = solve_allocation(param,g,I, false);
        %annrea = annealing(param,g,res.Ijk,'Il',min_mask,'Iu',max_mask);



        % Optimalq
        strcat("Started P_opt on ", datestr(datetime('now')))
        res_opt = optimal_network(param,g,I,min_mask,max_mask,x0);
      
        res_opt.welfare / res_stat.welfare


        param.K = 1.1;
        min_mask = I;
        strcat("Started P_10p on ", datestr(datetime('now')))
        res_opt_10p = optimal_network(param,g,I,min_mask,max_mask,x0);
        res_opt_10p.welfare / res_stat.welfare
     
%% mobile labor 
        
        K = 1.0;
        min_mask = adj .* 4.0;        
        max_mask = adj .* 120;
        param = init_parameters('Annealing', 'off', 'LaborMobility', 'on', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'off', 'rho', rho, 'K', K, 'CrossGoodCongestion', 'off');
        
        [param,g]=create_graph(param,[],[],'X',case_centroids.centroidx,'Y',case_centroids.centroidy,'Type','custom','Adjacency',adj);

        Zjn = productivity .* population; % this adapts the productivities to make sure it coincides with the a specified above
        % it also multiplies in another round of populations to adjust for
        % a weird bug in the toolbox where population gets normalised to 1
        Zjn(isnan(Zjn)) = 0;   

        
        param.Lj = population; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = Zjn;
        %param.omegaj = weights;
        
        param.Hj = population .* (1-alpha); % I normalise this because the general utility function has a (h_j/(1-alpha))^(1-alpha) thing with it
        param.tol_kappa = 1.0e-7;

        pop_target = population / sum(population);
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;


        converged = false;
        i = 1;

        x0 = [];
            while ~converged
                [res_statmob,flag,x0] = solve_allocation(param,g,I, false,x0);

                target_adjustment = pop_target ./ res_statmob.Lj;

                deviations = abs(target_adjustment - 1);
                deviations(pop_target < 0.00001) = 0.0;

                abs_diff = sum(deviations) ./ J;
                

                new_amenities = param.Hj.* target_adjustment;
                new_amenities(new_amenities == 0.0) = 10^-11;

                param.Hj = new_amenities;
                if abs_diff < 0.1 | i > 100
                    converged = true;
                end


                strcat(datestr(datetime('now')), ": ", " - ", string(i), ": Target deviation: ", string(abs_diff))            
                i = i +1;

            end
       


        % Static
        strcat("Started P_stat mobile on ", datestr(datetime('now')))
        %res_stat = optimal_network(param,g,I,I,I);
        [res_statmob,flag, x0] = solve_allocation(param,g,I, false);
        %annrea = annealing(param,g,res.Ijk,'Il',min_mask,'Iu',max_mask);



        % Optimalq
        strcat("Started P_opt mobile on ", datestr(datetime('now')))
        res_optmob = optimal_network(param,g,I,min_mask,max_mask,x0);
      
        res_optmob.welfare / res_statmob.welfare


        param.K = 1.1;
        min_mask = I;
        strcat("Started P_10p mobile on ", datestr(datetime('now')))
        res_opt_10pmob = optimal_network(param,g,I,min_mask,max_mask,x0);
        res_opt_10pmob.welfare / res_statmob.welfare

        %% Obtain descriptive statistics

        optimal_infrastructure = res_opt.Ijk;
%         raw_tradeflows_stat = sum(Q(P_stat, I, adj, delta_tau, beta, gamma), 3);
%         raw_tradeflows_opt = sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma),3);
% 
%         total_tradeflows_stat = sum(sum(sum(Q(P_stat, I, adj, delta_tau, beta, gamma))));
%         total_tradeflows_opt = sum(sum(sum(Q(P_opt, optimal_infrastructure, adj, delta_tau, beta, gamma))));

        util_stat = res_stat.uj;
        util_opt = res_opt.uj;
        util_opt_10p = res_opt_10p.uj;

        L_statmob = res_statmob.Lj;
        L_optmob = res_optmob.Lj;
        L_opt_10pmob = res_opt_10pmob.Lj;

      



        %% Export data

        % Optimal Network
        csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/" + ...
            "ken_hist/hist_outcomes/I_opt_", string(year), ".csv"), optimal_infrastructure);
        csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/" + ...
            "ken_hist/hist_outcomes/I_opt_10p_", string(year), ".csv"), res_opt_10p.Ijk);

        % Raw tradeflows and Optimal Tradeflows
        % csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Tradeflows/Initial_Flows_", (countryname), ".csv"), raw_tradeflows_stat);
        % csvwrite(strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/Tradeflows/Optimal_Flows_", (countryname), ".csv"), raw_tradeflows_opt);


        % Location Characteristics
        writetable(array2table([case_centroids.ID util_stat util_opt, util_opt_10p, L_statmob L_optmob, L_opt_10pmob], ...
          'VariableNames', {'ID', 'util_stat', 'util_opt', 'util_10p', 'L_statmob', 'L_optmob', 'L_10pmob'}), ...
          strcat("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/" + ...
          "hist_outcomes/outcomes_", string(year), ".csv"));

        year
        end
end


