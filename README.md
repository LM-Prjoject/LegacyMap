Step 1: Connect to the Supabase
- git clone https://github.com/LM-Prjoject/LegacyMap
- cd LegacyMap
- Access https://supabase.com/dashboard/account/tokens :
  + Click generate new token
  + Enter token name: Legacy
  + Choose Never expire
  + Copy token
- npx supabase login --token <TOKEN_NAME>
- npx supabase link --project-ref  ygwvctvwonegiyrhjjdp
- Open Docker
- npx supabase start

==================

Step 2: Working with DB
- Stand at LegacyMap folder
  + npx supabase migration new <mig_name>
  + write sql statements at the newly created file -> "Save"
  + npx supabase db push
! Note: After push into Supabase Cloud, pls commit to Git as usual. Thanks!
