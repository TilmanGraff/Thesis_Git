
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
a = 1.0; % production function parameter. Now in mobile labor it does matter, and I need to adjust productivities accordingly. Will do so below.
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...

outpath = "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/convex_test/"
inpath = "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/";

mkdir(outpath);
mkdir(strcat(outpath, "/Optimised_Networks/"))
mkdir(strcat(outpath, "/taus/"))
mkdir(strcat(outpath, "/Network_outcomes/"))
mkdir(strcat(outpath, "/amenities/"))


convex = true;
if convex 
    a = 1.23;
end


%% For each country


 for countryID = 1:length(country_names)
 %for countryID = 1:length(country_names)
    
     countryname = (country_names(countryID))
     if exist(strcat(inpath, "productivities/productivities_", (countryname), ".csv"), "file")  %&& ~exist(strcat(outpath, "/Network_outcomes/", (countryname), "_outcomes.csv"), "file")

        % Split centroids by country
        case_centroids = readtable(strcat(inpath, "borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
        if num_locations > 2 && num_locations < 100
        %if num_locations == 202
        %if countryname == "Botswana"
        % Read in characteristics
        population = case_centroids.pop;


        % Read in the relevant matrices
        adj = csvread(strcat(inpath, "adj/adj_", (countryname), ".csv"), 1, 0);
        abr = csvread(strcat(inpath, "abr/abr_", (countryname), ".csv"), 1, 0);
        delta_I = csvread(strcat(inpath, "delta_I/delta_I_", (countryname), ".csv"), 1, 0);

        %delta_tau = csvread(strcat(inpath, "delta_tau/delta_tau_withcomp_", (countryname), ".csv"), 1, 0);
        delta_tau = csvread(strcat(inpath, "delta_tau/delta_tau_", (countryname), ".csv"), 1, 0);
        %delta_tau = csvread(strcat(outpath, "delta_tau/delta_tau_fp_", (countryname), ".csv"), 1, 0);

        I = csvread(strcat(inpath, "I/I_", (countryname), ".csv"), 1, 0);
        productivity = csvread(strcat(inpath, "productivities/productivities_", (countryname), ".csv"), 1, 0);

        Zjn = productivity .* population.^(0.7-a) .* population; % this adapts the productivities to make sure it coincides with the a specified above
        Zjn(isnan(Zjn)) = 0;

        % Basic characteristics of the economy
        J = size(productivity, 1);
        N = size(productivity, 2);

        % Only optimise over the domestic grid cells
        weights = 1-case_centroids.abroad;

        K = 1.0; 
        
        
        %% Initialise geography

        if false %sum(case_centroids.abroad == 1) > 0
            nregions = 2;
            regions = case_centroids.abroad + 1;
            omegar = [1 0.1]'
            param = init_parameters('Annealing', 'off', 'LaborMobility', 'partial', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'on', 'rho', rho, 'K', K, 'CrossGoodCongestion', 'off');
            [param,g]=create_graph(param,[],[],'X',case_centroids.x,'Y',case_centroids.y,'Type','custom','Adjacency',adj,'X',case_centroids.x,'Y',case_centroids.y,  'NRegions', nregions, 'Region', regions);
        
  
            %param.omegar = omegar;

            fake_pop = population + (population == 0) .* 10^-1;
            
            param.Lj = fake_pop; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
            param.Zjn = Zjn;
    
            param.Lr = [sum(param.Lj .* ( 1.- case_centroids.abroad)), sum(param.Lj .* case_centroids.abroad)]' ./ sum(param.Lj);
            
            g.L = param.Lj


        else % basically just madagascar
            param = init_parameters('Annealing', 'off', 'LaborMobility', 'on', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'off', 'rho', rho, 'K', K,  'CrossGoodCongestion', 'off');
            [param,g]=create_graph(param,[],[],'X',case_centroids.x,'Y',case_centroids.y,'Type','custom','Adjacency',adj,'X',case_centroids.x,'Y',case_centroids.y);
            fake_pop = population + (population == 0) .* 10^-1;
            
            param.Lj = fake_pop; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
            param.Zjn = Zjn;
            param.omegaj = weights;
                %g.L = param.Lj
        end
        
                
        fake_pop = population + (population == 0) .* 10^-1;

        param.Lj = fake_pop; % this is needed as ipopt crashes otherwise (need to have population > 0 always), so I add an infinitisimal person
        param.Zjn = Zjn;

        
        pop_target = population / sum(population);
        
        param.Hj = fake_pop;  
        param.tol_kappa = 1.0e-4
        
        g.delta_i = delta_I;
        g.delta_tau = delta_tau;
        
        converged = false;
        i = 1;

        param.omegaj = weights;

        if param.mobility == 0
           param.Hj = ones(J,1)
            res_stat = solve_allocation(param,g,I, true);
        else

            strcat(countryname, ": Started P_stat with backout on ", datestr(datetime('now')))

x0 = [];
            while ~converged





                [res_stat,flag,x0] = solve_allocation(param,g,I, false,x0);

                target_adjustment = pop_target ./ res_stat.Lj;

                deviations = abs(target_adjustment - 1);
                deviations(pop_target < 0.0001) = 0.0;

                abs_diff = sum(deviations) ./ J;
                

                new_amenities = param.Hj.* target_adjustment;
                new_amenities(new_amenities == 0.0) = 10^-11;

                param.Hj = new_amenities;
                if abs_diff < 0.1 | i > 100
                    converged = true;
                end


                strcat(datestr(datetime('now')), ": ", countryname, " - ", string(i), ": Target deviation: ", string(abs_diff))            
                i = i +1;

            end
       
        csvwrite(strcat(inpath, "/amenities/", (countryname), ".csv"), param.Hj);
        end


        %% Optimisation
        
        if false %sum(case_centroids.abroad == 1) > 0
            param.omegar = omegar;
        end
     

        min_mask = abr .* I + (adj - abr) .* 4;        
        %min_mask = I; % this is for the 10perc exercise
        max_mask = abr .* I + (adj - abr) .* 120;


        if num_locations > 200
            param.tol_kappa = 1.0e-5
        end
        if num_locations > 400
            param.tol_kappa = 1.0e-4
        end
        if num_locations > 600
            param.tol_kappa = 1.0e-3
        end

        % Optimal
        strcat(countryname, ": Started P_opt on ", datestr(datetime('now')))
      
      if ~convex  
        res_opt = optimal_network(param,g,I,min_mask,max_mask,'true',x0);
      else
        res_opt = annealing(param,g,I,'Il',min_mask,'Iu',max_mask);
      end
     
        %% Obtain descriptive statistics

        optimal_infrastructure = res_opt.Ijk;

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


        consumption_stat = res_stat.Cj;
        consumption_opt = res_opt.Cj;
        
        price_index_stat = res_stat.PCj;
        price_index_opt = res_opt.PCj;
      
        %% Export data

        % Optimal Network
        csvwrite(strcat(outpath, "/Optimised_Networks/", (countryname), ".csv"), optimal_infrastructure);

        % Raw tradeflows and Optimal Tradeflows
        csvwrite(strcat(outpath, "/taus/tau_stat_", (countryname), ".csv"), tau_2D_stat);
        csvwrite(strcat(outpath, "/taus/tau_opt_", (countryname), ".csv"), tau_2D_opt);

        pop_opt = res_opt.Lj .* sum(population);

        % Location Characteristics
        writetable(array2table([case_centroids.ID case_centroids.x case_centroids.y case_centroids.abroad population pop_opt price_index_stat price_index_opt util_stat util_opt ...
          consumption_stat consumption_opt param.Hj], ...
          'VariableNames', {'ID', 'x', 'y', 'abroad', 'pop_stat', 'pop_opt', 'P_stat', 'P_opt', 'util_stat', 'util_opt', 'c_stat', 'c_opt', 'amenities'}), strcat(outpath, "/Network_outcomes/", (countryname), "_outcomes.csv"));

       end
    end
 end

