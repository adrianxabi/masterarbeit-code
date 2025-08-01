// openness_DITR_KOR_1997Q2.mod

var pih x y ynat rnat r s pi p ph e ystar pistar a deprec_rate;
varexo eps_star eps_a;
parameters sigma eta gamma phi epsilon theta beta alpha phi_pi rhoa rhoy sigma_a use_ditr use_citr;

// 1) Deep parameters
sigma   = 1;
eta     = 1;
gamma   = 1;
phi     = 3;
epsilon = 6;
theta   = 0.75;
beta    = 0.99;
phi_pi  = 1.5;
rhoy    = 0.86;

// 2) Calibrated for KOR 1997Q2
alpha   = 0.3802611888336371;
rhoa    = 0.42180977429361893;
sigma_a = 1.168914590404676;

// 3) Switch‐flags
use_ditr = 1;
use_citr = 0;

model(linear);

    // IS curve
    x = x(+1) - (1/sigma_a)*(r - pih(+1) - rnat);

    // Phillips curve
    pih = beta*pih(+1)
        + ( (1 - beta*theta)*(1 - theta)/theta
            * sigma
            / ( (1-alpha)
                + alpha*(sigma*gamma + (1-alpha)*(sigma*eta - 1)) )
          + phi
          ) * x;

    // Natural rate
    rnat = - sigma_a*(1 + phi)
           / ( sigma
               / ( (1-alpha)
                   + alpha*(sigma*gamma + (1-alpha)*(sigma*eta - 1)) )
               + phi
             )
           * (1 - rhoa) * a
          + alpha * sigma_a
            * ( (sigma*gamma - 1) + (1-alpha)*(sigma*eta - 1) )
            * ( ystar(+1) - ystar );

    // Natural output
    ynat = (1 + phi)
           / ( sigma
               / ( (1-alpha)
                   + alpha*(sigma*gamma + (1-alpha)*(sigma*eta - 1)) )
               + phi
             ) * a
          + alpha
            * ( - ( (sigma*gamma - 1) + (1-alpha)*(sigma*eta - 1) )
                * sigma
                / ( (1-alpha)
                    + alpha*(sigma*gamma + (1-alpha)*(sigma*eta - 1)) )
                / ( sigma
                    / ( (1-alpha)
                        + alpha*(sigma*gamma + (1-alpha)*(sigma*eta - 1)) )
                    + phi
                  )
              ) * ystar;

    // Output gap & dynamics
    x           = y - ynat;
    y           = ystar + (1/sigma_a)*s;
    pi          = pih + alpha*(s - s(-1));
    s           = s(-1) + e - e(-1) + pistar - pih;
    pistar      = 0;
    ystar       = rhoy*ystar(-1) + eps_star;
    a           = rhoa*a(-1)   + eps_a;

    // Policy: pure DITR
    r = phi_pi * pih;

    // Final definitions
    pi          = p - p(-1);
    pih         = ph - ph(-1);
    deprec_rate = e - e(-1);

end;

shocks;
    var eps_a; stderr 1;    // technology shock = 1 (i.e. 1 % in logs)
    var eps_star; stderr 0; // no foreign shock
    corr eps_a, eps_star = 0;
end;


stoch_simul(order=1, irf=20, irf_plot_threshold=0) pih x pi r s e;

stoch_simul(order=1,irf=20) pih x pi s e r ph p;

// at the end (after „end;“) …
save('openness_DITR_KOR_1997Q2.mat', 'oo_', 'M_', 'options_');