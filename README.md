Step 1: Connect to the Supabase
- git clone https://github.com/LM-Prjoject/LegacyMap
- cd LegacyMap
- npx supabase login
- Access https://supabase.com/dashboard/account/tokens :
  + Click generate new token
  + Enter token name: Legacy
  + Choose Never expire
  + Copy token
- npx supabase login --token <TOKEN_NAME>
- npx supabase link --project-ref  ygwvctvwonegiyrhjjdp
- Open Docker
- npx supabase start
