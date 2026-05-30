      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  39-ARGUMENTOS-ENTRADA.

       05  39-COLUNAS-DA-TABELA.
           10  39-CPRODT                   PIC S9(3)V COMP-3.
           10  39-DINIC-PARMZ-CONTR        PIC X(10).
           10  39-TMAX-CONTR-LIM           PIC S9(5)V COMP-3.
           10  39-CINDCD-PRZ-CONTR         PIC X(2).
           10  39-CINDCD-RENOV-AUTOM       PIC X(1).
           10  39-TCAREN-RENOV-CONTR       PIC S9(3)V COMP-3.
           10  39-CINDCD-PRZ-RENOV         PIC X(2).
           10  39-VMAX-RENOV-AUTOM         PIC S9(15)V9(2) COMP-3.
           10  39-CINDCD-TARIF-ADTTO       PIC X(1).
           10  39-CIDTFD-TARIF-ADTTO       PIC S9(5)V COMP-3.
           10  39-DFIM-PARMZ-CONTR         PIC X(10).
           10  39-HULT-ATULZ               PIC X(26).
           10  39-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  39-CTERM                    PIC X(8).
           10  39-CINDCD-ENVIO-SDO         PIC X(1).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  39-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  39-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  39-RETORNO.
       05  39-COD-RETORNO                         PIC 9(04).
       05  39-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  39-PROXIMO-RESTART                     PIC 9(05).
       05  39-QTDE-RETORNADA                      PIC 9(03).
       05  39-QTDE-TOTAL                          PIC 9(09).



      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  39-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T39.
           10  39RT-CPRODT                 PIC S9(3)V COMP-3.
           10  39RT-DINIC-PARMZ-CONTR      PIC X(10).
           10  39RT-TMAX-CONTR-LIM         PIC S9(5)V COMP-3.
           10  39RT-CINDCD-PRZ-CONTR       PIC X(2).
           10  39RT-CINDCD-RENOV-AUTOM     PIC X(1).
           10  39RT-TCAREN-RENOV-CONTR     PIC S9(3)V COMP-3.
           10  39RT-CINDCD-PRZ-RENOV       PIC X(2).
           10  39RT-VMAX-RENOV-AUTOM       PIC S9(15)V9(2) COMP-3.
           10  39RT-CINDCD-TARIF-ADTTO     PIC X(1).
           10  39RT-CIDTFD-TARIF-ADTTO     PIC S9(5)V COMP-3.
           10  39RT-DFIM-PARMZ-CONTR       PIC X(10).
           10  39RT-HULT-ATULZ             PIC X(26).
           10  39RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  39RT-CTERM                  PIC X(8).
           10  39RT-CINDCD-ENVIO-SDO       PIC X(1).

