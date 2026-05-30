      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB011 - DB2PRD.TCONTR_LIM_DESC        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  11-ARGUMENTOS-ENTRADA.

       05  11-COLUNAS-DA-TABELA.
           10  11-CCONTR-LIM-DESC              PIC S9(9)V COMP-3.
           10  11-CVRSAO-CONTR-LIM             PIC S9(3)V COMP-3.
           10  11-CINDCD-MODLD-DESC            PIC X(1).
           10  11-CBCO                         PIC S9(3)V COMP-3.
           10  11-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  11-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  11-CPOSTO-SERVC                 PIC S9(5)V COMP-3.
           10  11-CBCO-OPER-CONTR              PIC S9(3)V COMP-3.
           10  11-CAG-OPER-CONTR               PIC S9(5)V COMP-3.
           10  11-DINCL-CONTR-LIM              PIC X(10).
           10  11-DINIC-VGCIA-CONTR            PIC X(10).
           10  11-DVCTO-CONTR-LIM              PIC X(10).
           10  11-TCONTR-LIM-DESC              PIC S9(5)V COMP-3.
           10  11-CINDCD-PRZ-CONTR             PIC X(2).
           10  11-VCONTR-LIM-DESC              PIC S9(15)V9(2) COMP-3.
           10  11-VUTLZD-CONTR-LIM             PIC S9(15)V9(2) COMP-3.
           10  11-VDISPN-CONTR-LIM             PIC S9(15)V9(2) COMP-3.
           10  11-HATULZ-SDO-CONTR             PIC X(26).
           10  11-CINDCD-CONTR-ROTVO           PIC X(1).
           10  11-CINDCD-RENOV-AUTOM           PIC X(1).
           10  11-CINDCD-RTEIO-BNEFC           PIC X(1).
           10  11-CINDCD-LIBRC-CONTR           PIC X(1).
           10  11-CINDCD-TARIF-ADTTO           PIC X(1).
           10  11-CPRODT                       PIC S9(3)V COMP-3.
           10  11-DINIC-PARMZ-CONTR            PIC X(10).
           10  11-CTPO-COBR-TAC                PIC S9(3)V COMP-3.
           10  11-CINDCD-TAC-SPROD             PIC X(1).
           10  11-VTAC-CONTR-LIM               PIC S9(15)V9(2) COMP-3.
           10  11-DTAC-CONTR-LIM               PIC X(10).
           10  11-CINDCD-TPO-RENOV             PIC X(1).
           10  11-DRENOV-CONTR-LIM             PIC X(10).
           10  11-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  11-HSIT-DESC-COML               PIC X(26).
           10  11-DULT-UTILZ-CONTR             PIC X(10).
           10  11-DULT-ADTTO-CONTR             PIC X(10).
           10  11-CINDCD-MOTVO-ADTTO           PIC X(1).
           10  11-VTARIF-ADTTO-CONTR           PIC S9(15)V9(2) COMP-3.
           10  11-DCOBR-TARIF-ADTTO            PIC X(10).
FS2511     10  11-CCNPJ-CPF                    PIC X(09).
FS2511     10  11-CFLIAL-CNPJ                  PIC X(04).
           10  11-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  11-CCLUB                        PIC S9(10)V COMP-3.
           10  11-HULT-ATULZ                   PIC X(26).
           10  11-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  11-CTERM                        PIC X(8).
           10  11-CINDCD-OPER-INADP            PIC X(1).
           10  11-DATULZ-OPER-INADP            PIC X(10).
           10  11-CCHAVE-ELMTO-DESC            PIC X(45).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  11-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  11-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  11-RETORNO.
       05  11-COD-RETORNO                         PIC 9(04).
       05  11-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  11-PROXIMO-RESTART                     PIC 9(05).
       05  11-QTDE-RETORNADA                      PIC 9(03).
       05  11-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  11-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T11.
           10  11RT-CCONTR-LIM-DESC            PIC S9(9)V COMP-3.
           10  11RT-CVRSAO-CONTR-LIM           PIC S9(3)V COMP-3.
           10  11RT-CINDCD-MODLD-DESC          PIC X(1).
           10  11RT-CBCO                       PIC S9(3)V COMP-3.
           10  11RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  11RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  11RT-CPOSTO-SERVC               PIC S9(5)V COMP-3.
           10  11RT-CBCO-OPER-CONTR            PIC S9(3)V COMP-3.
           10  11RT-CAG-OPER-CONTR             PIC S9(5)V COMP-3.
           10  11RT-DINCL-CONTR-LIM            PIC X(10).
           10  11RT-DINIC-VGCIA-CONTR          PIC X(10).
           10  11RT-DVCTO-CONTR-LIM            PIC X(10).
           10  11RT-TCONTR-LIM-DESC            PIC S9(5)V COMP-3.
           10  11RT-CINDCD-PRZ-CONTR           PIC X(2).
           10  11RT-VCONTR-LIM-DESC            PIC S9(15)V9(2) COMP-3.
           10  11RT-VUTLZD-CONTR-LIM           PIC S9(15)V9(2) COMP-3.
           10  11RT-VDISPN-CONTR-LIM           PIC S9(15)V9(2) COMP-3.
           10  11RT-HATULZ-SDO-CONTR           PIC X(26).
           10  11RT-CINDCD-CONTR-ROTVO         PIC X(1).
           10  11RT-CINDCD-RENOV-AUTOM         PIC X(1).
           10  11RT-CINDCD-RTEIO-BNEFC         PIC X(1).
           10  11RT-CINDCD-LIBRC-CONTR         PIC X(1).
           10  11RT-CINDCD-TARIF-ADTTO         PIC X(1).
           10  11RT-CPRODT                     PIC S9(3)V COMP-3.
           10  11RT-DINIC-PARMZ-CONTR          PIC X(10).
           10  11RT-CTPO-COBR-TAC              PIC S9(3)V COMP-3.
           10  11RT-CINDCD-TAC-SPROD           PIC X(1).
           10  11RT-VTAC-CONTR-LIM             PIC S9(15)V9(2) COMP-3.
           10  11RT-DTAC-CONTR-LIM             PIC X(10).
           10  11RT-CINDCD-TPO-RENOV           PIC X(1).
           10  11RT-DRENOV-CONTR-LIM           PIC X(10).
           10  11RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  11RT-HSIT-DESC-COML             PIC X(26).
           10  11RT-DULT-UTILZ-CONTR           PIC X(10).
           10  11RT-DULT-ADTTO-CONTR           PIC X(10).
           10  11RT-CINDCD-MOTVO-ADTTO         PIC X(1).
           10  11RT-VTARIF-ADTTO-CONTR         PIC S9(15)V9(2) COMP-3.
           10  11RT-DCOBR-TARIF-ADTTO          PIC X(10).
FS2511     10  11RT-CCNPJ-CPF                  PIC X(09).
FS2511     10  11RT-CFLIAL-CNPJ                PIC X(04).
           10  11RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  11RT-CCLUB                      PIC S9(10)V COMP-3.
           10  11RT-HULT-ATULZ                 PIC X(26).
           10  11RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  11RT-CTERM                      PIC X(8).
           10  11RT-CINDCD-OPER-INADP          PIC X(1).
           10  11RT-DATULZ-OPER-INADP          PIC X(10).
           10  11RT-CCHAVE-ELMTO-DESC          PIC X(45).
