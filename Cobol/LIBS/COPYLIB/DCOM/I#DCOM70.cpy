      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  70-ARGUMENTOS-ENTRADA.

       05  70-COLUNAS-DA-TABELA.
           10 70-CPRODT               PIC S9(3)V USAGE COMP-3.
           10 70-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10 70-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
           10 70-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
           10 70-HINIC-VGCIA-DESC     PIC X(26).
           10 70-HFIM-VGCIA-DESC      PIC X(26).
           10 70-HULT-ATULZ           PIC X(26).
           10 70-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10 70-CTERM                PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  70-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  70-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  70-RETORNO.
       05  70-COD-RETORNO                         PIC 9(04).
       05  70-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  70-PROXIMO-RESTART                     PIC 9(05).
       05  70-QTDE-RETORNADA                      PIC 9(03).
       05  70-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  70-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T70.
           10 70RT-CPRODT               PIC S9(3)V USAGE COMP-3.
           10 70RT-CSPROD-DESC-COML     PIC S9(3)V USAGE COMP-3.
           10 70RT-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
           10 70RT-CIDTFD-PRODT-COBR    PIC S9(2)V USAGE COMP-3.
           10 70RT-HINIC-VGCIA-DESC     PIC X(26).
           10 70RT-HFIM-VGCIA-DESC      PIC X(26).
           10 70RT-HULT-ATULZ           PIC X(26).
           10 70RT-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           10 70RT-CTERM                PIC X(8).
