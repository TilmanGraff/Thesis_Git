function [tradeflows] = Q(P, I, adj, delta_tau, beta, gamma)

J = size(P, 1);
N = size(P, 2);


P_transform = reshape(P, [J, 1, N]);

for n=1:N
   P_n = P_transform(:,:,n);
   
   tradeflows(:,:,n) = (((1/(1+beta)) .* (I.^gamma).*((delta_tau).^-1) .* max(zeros(J), transpose(P_n * transpose(P_n.^-1)) -1)).^(1/beta)) .* adj;
   
   
end

tradeflows(isnan(tradeflows)) = 0;

end

