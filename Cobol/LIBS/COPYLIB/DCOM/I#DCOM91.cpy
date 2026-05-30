      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
DCOM01* ALTERACAO: 01/08/2023 - LUCIANDRA - WIPRO                     *
DCOM01* OBJETIVO.: INCLUIR PRODUTO CDPS E INDICADOR DE COOBRIGACAO    *
DCOM01*****************************************************************
       01  91-ARGUMENTOS-ENTRADA.

       05  91-COLUNAS-DA-TABELA.
           10  91-CPRODT                   PIC S9(3)V COMP-3.
           10  91-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  91-DINIC-VGCIA-SPROD        PIC X(10).
           10  91-DFIM-VGCIA-SPROD         PIC X(10).
           10  91-CTPO-DESC-COML           PIC S9(3)V COMP-3.
           10  91-ISPROD-DESC-COML         PIC X(40).
           10  91-IRSUMO-SPROD-DESC        PIC X(10).
           10  91-CSGL-SPROD-DESC          PIC X(6).
           10  91-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  91-HSIT-DESC-COML           PIC X(26).
           10  91-CINDCD-VALDC-ROTRO       PIC X(1).
           10  91-DVALDC-ROTRO-ELMTO       PIC X(10).
           10  91-CFUNC-VALDC-ROTRO        PIC S9(9)V COMP-3.
           10  91-RSPROD-DESC-COML.
              49  91-RSPROD-DESC-COML-LEN  PIC S9(4) COMP.
              49  91-RSPROD-DESC-COML-TEXT PIC X(240).
           10  91-CPRODT-BASE-INCL         PIC S9(3)V COMP-3.
           10  91-CSPROD-BASE-INCL         PIC S9(3)V COMP-3.
           10  91-RJUSTF-SIT-SPROD.
              49  91-RJUSTF-SIT-SPROD-LEN  PIC S9(4) COMP.
              49  91-RJUSTF-SIT-SPROD-TEXT PIC X(240).
           10  91-HULT-ATULZ               PIC X(26).
           10  91-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  91-CTERM                    PIC X(8).
DCOM01     10  91-CPRODT-SERVC-OPER        PIC S9(8)V COMP-3.
DCOM01     10  91-CCOOBC-PRODT-DESC        PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  91-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  91-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  91-RETORNO.
       05  91-COD-RETORNO                         PIC 9(04).
       05  91-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  91-PROXIMO-RESTART                     PIC 9(05).
       05  91-QTDE-RETORNADA                      PIC 9(03).
       05  91-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  91-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T91.
           10  91RT-CPRODT                    PIC S9(3)V COMP-3.
           10  91RT-CSPROD-DESC-COML          PIC S9(3)V COMP-3.
           10  91RT-DINIC-VGCIA-SPROD         PIC X(10).
           10  91RT-DFIM-VGCIA-SPROD          PIC X(10).
           10  91RT-CTPO-DESC-COML            PIC S9(3)V COMP-3.
           10  91RT-ISPROD-DESC-COML          PIC X(40).
           10  91RT-IRSUMO-SPROD-DESC         PIC X(10).
           10  91RT-CSGL-SPROD-DESC           PIC X(6).
           10  91RT-CSIT-DESC-COML            PIC S9(3)V COMP-3.
           10  91RT-HSIT-DESC-COML            PIC X(26).
           10  91RT-CINDCD-VALDC-ROTRO        PIC X(1).
           10  91RT-DVALDC-ROTRO-ELMTO        PIC X(10).
           10  91RT-CFUNC-VALDC-ROTRO         PIC S9(9)V COMP-3.
           10  91RT-RSPROD-DESC-COML.
              49  91RT-RSPROD-DESC-COML-LEN   PIC S9(4) COMP.
              49  91RT-RSPROD-DESC-COML-TEXT  PIC X(240).
           10  91RT-CPRODT-BASE-INCL          PIC S9(3)V COMP-3.
           10  91RT-CSPROD-BASE-INCL          PIC S9(3)V COMP-3.
           10  91RT-RJUSTF-SIT-SPROD.
              49  91RT-RJUSTF-SIT-SPROD-LEN   PIC S9(4) COMP.
              49  91RT-RJUSTF-SIT-SPROD-TEXT  PIC X(240).
           10  91RT-HULT-ATULZ                PIC X(26).
           10  91RT-CFUNC-BDSCO               PIC S9(9)V COMP-3.
           10  91RT-CTERM                     PIC X(8).
DCOM01     10  91RT-CPRODT-SERVC-OPER         PIC S9(8)V COMP-3.
DCOM01     10  91RT-CCOOBC-PRODT-DESC         PIC X(1).
