
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

gamma = 0.27;
gamma = 0.9;
beta = 1.2446 * gamma;

% beta = 1.2446 * gamma;
sigma = 5;
a = 1.0; % production function parameter. Now in mobile labor it does matter, and I need to adjust productivities accordingly. Will do so below.
rho = 0; % this is really important to not have any inequality aversion. I am not entirely sure if thats legit because in their toolbox, FS say rho >= 1... but i see no reason why 0 should not be ok...

outpath = "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/"
inpath = "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/temp/";
%% For each country


 for countryID = 1:length(country_names)
 %for countryID = 1:length(country_names)
    
     countryname = (country_names(countryID))
     if exist(strcat(inpath, "productivities/productivities_", (countryname), ".csv"), "file")  %&& ~exist(strcat(outpath, "/Network_outcomes/", (countryname), "_outcomes.csv"), "file")

        % Split centroids by country
        case_centroids = readtable(strcat(inpath, "borderregions/", (countryname), "_borderregion.csv"));
        num_locations = size(case_centroids, 1)
        %if num_locations > 50 && num_locations < 80
        %if num_locations == 202
        if countryname == "Botswana"
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
            param = init_parameters('Annealing', 'off', 'LaborMobility', 'on', 'a', a, 'sigma', sigma, 'N', N, 'alpha', alpha, 'beta', beta, 'gamma', gamma, 'verbose', 'on', 'rho', rho, 'K', K,  'CrossGoodCongestion', 'off');
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
%%
% has_calibration_converged=false;
% counter=0;
% tol_L=1e-2;
% tol_GDP_per_cap=1e-2;
% tol_intra_reg_trade_share=0.5e-2;
% tol_trade_dist_elas=1e-2; % put 3e-2 if target elasticity -1
% MAX_ITER=400;
% step_Z=0.95;
% step_L=0.75;
% step_d0=0.95;
% step_d1=0.95;
% display_details = false;
% save_before_it_crashes = false;
% 
%     % --------------------------------------------
% % Identify which good is produced by each city
% [~,produced_good_id] = max(param.Zjn,[],2);
% d1=.85;
% 
%                 Cn=sum(param.Zjn.*g.L(:,ones(param.N,1)).^param.a,1); % total quantity produced of each good
%                 Pn=((Cn/Cn(1)).^(-1/param.sigma))';
% 
%                 % Zu = 1e2*g.Y./(Pn(produced_good_id).*g.L.^(param.a)); % upper bound on productivity
%                 % Zl = 1e-2*g.Y./(Pn(produced_good_id).*g.L.^(param.a)); % lower bound
% 
%                 hu=1e2*ones(g.J,1); % upper bound on housing per capita
%                 hl=1e-4*ones(g.J,1); % lower bound
% 
% d1u=d1*1.2; % upper bound on delta1
% d1l=d1/1.2; % lower bound
% 
% 
% 
% x0=[]; % we init optimization from previous optimum, empty for the first run
% best_fit.score=inf;
% 
% 
% skip_update=false; % dummy indicating whether to skip updating once (debugging)
% attempts=0;
% Z_MEAN=10; % normalization (nominal GDP is normalized afterwards), chose>1 for numerical reasons
% counter = 0;
% has_L_converged = false;
% 
% target_L = g.L;
% 
% while ~has_L_converged && counter<MAX_ITER
% 
%     % Evaluate at current middle poin
% 
%     hcurrent = exp(0.5*log(hu)+0.5*log(hl));
%     param.hj = hcurrent;
%     param.Hj = param.hj .* target_L;
% 
%     %d0current=exp(0.5*log(d0u)+0.5*log(d0l));
%     d1current=exp(0.5*log(d1u)+0.5*log(d1l));
% 
%     % Compute tau or kappa given d0current
%     %g.delta_tau = d0current* ( g.distance ).^d1current / avg_dist^d1current;
% 
%     % % Solve allocation
%     % if save_before_it_crashes==true
%     %     save(debug_file_str,'param','g','x0','Zu','Zl','Zcurrent','hu','hl','hcurrent','d0u','d0l','d0current','d1u','d1l','d1current');
%     % end
% 
%     t0=clock();
%     [results,flag,x1] = solve_allocation(param,g,I,false,x0);
%     t1=clock();
% 
%     % The following deals with some failure of replicability that we have
%     % found using IPOPT:
%     if ~any( flag.status == [0,1] )
% 
%         x1=[]; % try again with default seed
%         skip_update=true;
%         attempts=attempts+1;
%         if attempts==3 % third time it fails, return error
%             error('%s.m: IPOPT returned with error code %d.',mfilename,flag.status);
%         end
%     else
%         attempts=0;
%     end
% 
%     if skip_update==true && any( flag.status == [0,1] )
%         skip_update=false;
%     end
% 
%     % Compute statistics
%     PHj = param.alpha / (1-param.alpha) * results.PCj .* results.hj ./ results.cj;
%     GDPj = sum(results.Pjn.*results.Yjn,2)+PHj.*param.Hj;
%     GDPj = GDPj/sum(GDPj); % normalize nominal GDP by 1
%     GDP_per_cap_j = GDPj./results.Lj;
%     GDP_per_cap_j(results.Lj==0) = 0;
% 
% 
%     % if isfield(g,'nuts')
%     %     [~,intra_reg_trade_share] = compute_trade_shares(param,g,results,1); % first is all, second is differentiated
%     % end
%     % 
%     % % compute trade-distance elasticity
%     % if compute_trade_dist_elas==true
%     %     trade_dist_elas = trade_dist_elasticity( results,param,g,GDPj );
%     % else
%     %     trade_dist_elas = 0; % just set trade_dist_elas to 0
%     % end
% 
%     % Evaluate fit
%     % diff_GDP_per_cap=(GDP_per_cap_j-target_GDP_per_cap)./target_GDP_per_cap;
%     % diff_GDP_per_cap(target_GDP_per_cap==0)=0; % the places with 0 income maintain 0 income throughout
%     % distance_GDP_per_cap=max(abs(diff_GDP_per_cap(~exclude_locations)));
%     % score=distance_GDP_per_cap;
%     % has_GDP_converged=distance_GDP_per_cap<tol_GDP_per_cap;
%     % has_calibration_converged = has_GDP_converged;    
%     % 
% 
%         diff_L=results.Lj-(target_L ./ sum(population));
%         distance_L=max(abs(diff_L));
%         score=distance_L;
%         mean(diff_L)
%         has_L_converged=distance_L<tol_L;
% 
%     % 
%     % 
%     % if isfield(g,'nuts')
%     %     diff_intra_reg_trade_share = intra_reg_trade_share-target_intra_reg_trade_share;
%     %     distance_intra_reg_trade_share = abs(diff_intra_reg_trade_share);
%     %     score=score+distance_intra_reg_trade_share;
%     % end
%     % if calibrate_d0==true
%     %     has_intra_reg_trade_share_converged = distance_intra_reg_trade_share < tol_intra_reg_trade_share;
%     %     has_calibration_converged=has_calibration_converged & has_intra_reg_trade_share_converged;
%     % end
% 
%     % if calibrate_d1==true
%     %     diff_trade_dist_elas = trade_dist_elas-target_trade_dist_elas;
%     %     distance_trade_dist_elas = abs(diff_trade_dist_elas);
%     %     score=score+distance_trade_dist_elas;
%     %     has_trade_dist_elas_converged = distance_trade_dist_elas < tol_trade_dist_elas;
%     % 
%     %     has_calibration_converged=has_calibration_converged & has_trade_dist_elas_converged;
%     % end
% 
%     % Update bounds
%     % if ~has_GDP_converged && skip_update==false
%     %     Ipos=find(diff_GDP_per_cap>tol_GDP_per_cap & ~exclude_locations);
%     %     Ineg=find(diff_GDP_per_cap<-tol_GDP_per_cap & ~exclude_locations);
%     % 
%     %     Zu(Ipos)=Zcurrent(Ipos)+step_Z*(Zu(Ipos)-Zcurrent(Ipos)); % bring the bounds closer together slowly
%     %     Zl(Ineg)=Zcurrent(Ineg)-step_Z*(Zcurrent(Ineg)-Zl(Ineg));
%     % end
% 
%         if ~has_L_converged %&& skip_update==false
%             Ipos=find(diff_L>tol_L);
%             Ineg=find(diff_L<-tol_L);
% 
%             hu(Ipos)=hcurrent(Ipos)+step_L*(hu(Ipos)-hcurrent(Ipos)); % bring the bounds closer together slowly
%             hl(Ineg)=hcurrent(Ineg)-step_L*(hcurrent(Ineg)-hl(Ineg));
%         end
% 
% 
%     % if calibrate_d0==true && skip_update==false
%     %     if ~has_intra_reg_trade_share_converged && distance_GDP_per_cap<0.05
%     %         if diff_intra_reg_trade_share>tol_intra_reg_trade_share
%     %             d0u = d0current + step_d0*(d0u-d0current);
%     %         end
%     % 
%     %         if diff_intra_reg_trade_share<-tol_intra_reg_trade_share
%     %             d0l = d0current - step_d0*(d0current-d0l);
%     %         end
%     %     end
%     % end
%     % 
%     % if calibrate_d1==true && skip_update==false
%     %     if ~has_trade_dist_elas_converged && max(abs(diff_GDP_per_cap))<0.05 && distance_intra_reg_trade_share < 0.02
%     %         if diff_trade_dist_elas>tol_trade_dist_elas
%     %             d1l = d1current - step_d1*(d1current-d1l);
%     %         end
%     % 
%     %         if diff_trade_dist_elas<-tol_trade_dist_elas
%     %             d1u = d1current + step_d1*(d1u-d1current);
%     %         end
%     %     end
%     % end
% 
%     if score<best_fit.score
%         best_fit.results=results;
%         best_fit.score=score;
%         best_fit.Zjn=param.Zjn;
%         best_fit.hj=param.hj;
%         best_fit.Hj=param.Hj;
%         % best_fit.delta_tau=g.delta_tau;
%         % best_fit.d0=d0current/avg_dist^d1current;
%         % best_fit.d1=d1current;
%         % if isfield(g,'nuts')
%         %     best_fit.intra_reg_trade_share = intra_reg_trade_share;
%         % end
%         % best_fit.trade_dist_elas = trade_dist_elas;
%     end
% 
%     counter=counter+1;
%     x0=x1;
% 
%     old_score=score;
%     old_param=param;
%     old_results=results;
%     %old_GDP_per_cap_j = GDP_per_cap_j;
% 
%     % Display iteration results
% 
%     fprintf('CALIBRATION: iteration No.%d...\n',counter);
%     vars = {'L'};
%     % if param.mobility==true
%     %     vars = [vars;'L'];
%     % end
% 
%     % if isfield(g,'nuts')
%     %     vars = [vars;'intra_reg_trade_share'];
%     % end
%     % 
%     % if calibrate_d1==true
%     %     vars = [vars;'trade_dist_elas'];
%     % end
% 
%     str = 'Fit max distance [';
%     for k=1:length(vars)
%         str = [str,vars{k}];
%         if k<length(vars)
%             str = [str,' '];
%         end
%     end
%     str = [str,'] = ['];
%     for k=1:length(vars)
%         str = [str,sprintf('%2.3f',eval(['distance_',vars{k}]))];
%         if k<length(vars)
%             str = [str,' '];
%         end
%     end
%     str = [str,']'];
% 
%     disp(str);
% 
%     % if isfield(g,'nuts')
%     %     fprintf('Moments: intra_reg_trade_share=%.3f, trade_dist_elas=%.3f\n',intra_reg_trade_share,trade_dist_elas);
%     % else
%     %     fprintf('Moments: trade_dist_elas=%.3f\n',trade_dist_elas);
%     % end
%     % fprintf('Current estimates: d0=%.5f, d1=%.3f\n',d0current/avg_dist^d1current,d1current);
%     % if display_details==true
%     %     fprintf('GDP_per_capita fit [model data diff]:\n');
%     %     [GDP_per_cap_j target_GDP_per_cap diff_GDP_per_cap]
%     % 
%     %     if param.mobility==true
%     %         fprintf('Population fit:\n');
%     %         [results.Lj target_L diff_L]
%     %     end
%     % end
%     fprintf('Computation time: %3.1f secs.\n',etime(t1,t0));
% end
% res_stat = solve_allocation(param,g,I, false);
%%


x0 = [];
            while ~converged





                [res_stat,flag,x0] = solve_allocation(param,g,I, true,x0);

                target_adjustment = pop_target ./ res_stat.Lj;

                deviations = abs(target_adjustment - 1);
                deviations(pop_target < 0.0001) = 0.0;

                abs_diff = sum(deviations) ./ J;
                

                new_amenities = param.Hj.* target_adjustment;
                new_amenities(new_amenities == 0.0) = 10^-11;

                param.Hj = new_amenities;
                if abs_diff < 0.0001 | i > 100
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
      
        
        res_opt = optimal_network(param,g,I,min_mask,max_mask,'true',x0);
        
     
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
