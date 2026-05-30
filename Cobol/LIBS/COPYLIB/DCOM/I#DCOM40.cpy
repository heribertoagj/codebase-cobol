      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  40-ARGUMENTOS-ENTRADA.

       05  40-COLUNAS-DA-TABELA.
           10  40-CPRODT                   PIC S9(3)V COMP-3.
           10  40-DINIC-PARMZ-PCELA        PIC X(10).
           10  40-TTRNSF-CREDT-ATRSO       PIC S9(3)V COMP-3.
           10  40-CINDCD-PRZ-MORA          PIC X(2).
           10  40-VMAX-RESDU-PCELA         PIC S9(15)V9(2) COMP-3.
           10  40-CINDCD-DEB-RESDU         PIC X(1).
           10  40-DFIM-PARMZ-PCELA         PIC X(10).
           10  40-CIDTFD-CUSTO-MORA        PIC 9(05).
           10  40-CIDTFD-CUSTO-CRATS       PIC 9(05).
           10  40-HULT-ATULZ               PIC X(26).
           10  40-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  40-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  40-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  40-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  40-RETORNO.
       05  40-COD-RETORNO                         PIC 9(04).
       05  40-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  40-PROXIMO-RESTART                     PIC 9(05).
       05  40-QTDE-RETORNADA                      PIC 9(03).
       05  40-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  40-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T40.
           10  40RT-CPRODT                 PIC S9(3)V COMP-3.
           10  40RT-DINIC-PARMZ-PCELA      PIC X(10).
           10  40RT-TTRNSF-CREDT-ATRSO     PIC S9(3)V COMP-3.
           10  40RT-CINDCD-PRZ-MORA        PIC X(2).
           10  40RT-VMAX-RESDU-PCELA       PIC S9(15)V9(2) COMP-3.
           10  40RT-CINDCD-DEB-RESDU       PIC X(1).
           10  40RT-DFIM-PARMZ-PCELA       PIC X(10).
           10  40RT-CIDTFD-CUSTO-MORA      PIC 9(05).
           10  40RT-CIDTFD-CUSTO-CRATS     PIC 9(05).
           10  40RT-HULT-ATULZ             PIC X(26).
           10  40RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  40RT-CTERM                  PIC X(8).
