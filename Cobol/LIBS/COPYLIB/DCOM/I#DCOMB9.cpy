      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B9 - DB2PRD.THIST_SPROD_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  B9-ARGUMENTOS-ENTRADA.

       05  B9-COLUNAS-DA-TABELA.
           10  B9-CPRODT                       PIC S9(3)V COMP-3.
           10  B9-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  B9-HULT-ATULZ                   PIC X(26).
           10  B9-DINIC-VGCIA-SPROD            PIC X(10).
           10  B9-DFIM-VGCIA-SPROD             PIC X(10).
           10  B9-CTPO-DESC-COML               PIC S9(3)V COMP-3.
           10  B9-ISPROD-DESC-COML             PIC X(40).
           10  B9-IRSUMO-SPROD-DESC            PIC X(10).
           10  B9-CSGL-SPROD-DESC              PIC X(6).
           10  B9-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  B9-HSIT-DESC-COML               PIC X(26).
           10  B9-CINDCD-VALDC-ROTRO           PIC X(1).
           10  B9-DVALDC-ROTRO-ELMTO           PIC X(10).
           10  B9-CFUNC-VALDC-ROTRO            PIC S9(9)V COMP-3.
           10  B9-RSPROD-DESC-COML.
              49  B9-RSPROD-DESC-COML-LEN      PIC S9(4) COMP.
              49  B9-RSPROD-DESC-COML-TEXT     PIC X(240).
           10  B9-CPRODT-BASE-INCL             PIC S9(3)V COMP-3.
           10  B9-CSPROD-BASE-INCL             PIC S9(3)V COMP-3.
           10  B9-RJUSTF-SIT-SPROD.
              49  B9-RJUSTF-SIT-SPROD-LEN      PIC S9(4) COMP.
              49  B9-RJUSTF-SIT-SPROD-TEXT     PIC X(240).
           10  B9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B9-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B9-RETORNO.
       05  B9-COD-RETORNO                         PIC 9(04).
       05  B9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B9-PROXIMO-RESTART                     PIC 9(05).
       05  B9-QTDE-RETORNADA                      PIC 9(03).
       05  B9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB9.
           10  B9RT-CPRODT                     PIC S9(3)V COMP-3.
           10  B9RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  B9RT-HULT-ATULZ                 PIC X(26).
           10  B9RT-DINIC-VGCIA-SPROD          PIC X(10).
           10  B9RT-DFIM-VGCIA-SPROD           PIC X(10).
           10  B9RT-CTPO-DESC-COML             PIC S9(3)V COMP-3.
           10  B9RT-ISPROD-DESC-COML           PIC X(40).
           10  B9RT-IRSUMO-SPROD-DESC          PIC X(10).
           10  B9RT-CSGL-SPROD-DESC            PIC X(6).
           10  B9RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  B9RT-HSIT-DESC-COML             PIC X(26).
           10  B9RT-CINDCD-VALDC-ROTRO         PIC X(1).
           10  B9RT-DVALDC-ROTRO-ELMTO         PIC X(10).
           10  B9RT-CFUNC-VALDC-ROTRO          PIC S9(9)V COMP-3.
           10  B9RT-RSPROD-DESC-COML.
              49  B9RT-RSPROD-DESC-COML-LEN    PIC S9(4) COMP.
              49  B9RT-RSPROD-DESC-COML-TEXT   PIC X(240).
           10  B9RT-CPRODT-BASE-INCL           PIC S9(3)V COMP-3.
           10  B9RT-CSPROD-BASE-INCL           PIC S9(3)V COMP-3.
           10  B9RT-RJUSTF-SIT-SPROD.
              49  B9RT-RJUSTF-SIT-SPROD-LEN    PIC S9(4) COMP.
              49  B9RT-RJUSTF-SIT-SPROD-TEXT   PIC X(240).
           10  B9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B9RT-CTERM                      PIC X(8).
