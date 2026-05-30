
SELECT  SUM(VSDO_DVDOR_PRINC),
        SUM(VTOT_ENCAR),      
        SUM(VTOT_JURO_MORA),  
        SUM(VTOT_MULTA)      
  FROM DB2PRD.TPCELA_TEMPR_RENEG
 WHERE CJUNC_DEPDC = 02028
   AND CCDULA = 202605149     


SELECT
    r.B003_CH_CONTR AS CCONTR,
    DECIMAL(
        COALESCE(
            r.B003_VLAUXIL_OPER9
          + (r.B003_JURPGAC_PAG8  - r.B003_JURPGORI_CAP9)
          + (r.B003_CORRPGAC_PAG8 - r.B003_CORRPGOR_CAP9)
        , 0)
      + COALESCE(p.SOMA_PARC_JURO, 0)
    , 15, 2) AS SDVATU
FROM DB2PRD.RUCA_CAD_ROOT r
LEFT JOIN (
    SELECT
        CCONTR,
        VALUE(SUM(VPCELA) + SUM(VPROVS_JURO), 0) AS SOMA_PARC_JURO
    FROM DB2PRD.V01PARCELA_PGTO
    GROUP BY CCONTR
) p
    ON p.CCONTR = r.B003_CH_CONTR
WHERE r.B003_CH_CONTR = 0087200447;

A system abend 0C7 reason code X'0' occurred in module RUEC7764 program RUEC7764
at offset X'3590'.                                                              


SELECT VALUE ( SUM(VPCELA) +  SUM(VPROVS_JURO) , 0 )
FROM   DB2PRD.V01PARCELA_PGTO                       
WHERE  CCONTR              = 8720447   

              COL1
------------------
            250,45


SELECT B003_VLAUXIL_OPER9                         
    + (B003_JURPGAC_PAG8  -  B003_JURPGORI_CAP9 ) 
    + (B003_CORRPGAC_PAG8 -  B003_CORRPGOR_CAP9 ) 
    , (B003_JURPGAC_PAG8  -  B003_JURPGORI_CAP9 ) 
    , B003_JURPGORI_CAP9                          
FROM  DB2PRD.RUCA_CAD_ROOT                        
WHERE B003_CH_CONTR      = 8720447    
              COL1                COL2              CAP9  
------------------  ------------------  ------------------
          13929,75                6,28              
