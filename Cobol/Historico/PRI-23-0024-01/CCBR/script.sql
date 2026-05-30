
-- Gerar dados detalhe
    INSERT INTO DB2PRD.TDETLH_PROCM_RECTA_FEDRL
    VALUE   ( 201501, 
            (SELECT VALUE(MAX(NDETLH_PROCM_RECTA) + 1, 1)
               FROM DB2PRD.TDETLH_PROCM_RECTA_FEDRL) ,
               1 , 'CARGA AUT', CURRENT_TIMESTAMP, ' ',
               'CARGA AUT', '0001-01-01-00.00.00.000000',
               0, ' ', NULL, NULL ) 

-- Gerar dados de inconsistencia
--
    INSERT INTO DB2PRD.TINCON_PROCM_RECTA_FEDRL
    VALUES  (202303, 
            (SELECT NDETLH_PROCM_RECTA 
               FROM DB2PRD.TDETLH_PROCM_RECTA_FEDRL
               WHERE DANO_MES_REFT = 202303
                 AND CSIT_PROCM_RECTA = 4 
               FETCH FIRST ROW ONLY), 
            (SELECT VALUE(MAX(NINCON_PROCM_INFO) + 1, 1)
               FROM DB2PRD.TINCON_PROCM_RECTA_FEDRL), 
            '12345678000101', 
            01,
            'CCBR', 1, 2, 3, CURRENT_TIMESTAMP)
