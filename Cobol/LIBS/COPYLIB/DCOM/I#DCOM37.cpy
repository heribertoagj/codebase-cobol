      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  37-ARGUMENTOS-ENTRADA.

       05  37-COLUNAS-DA-TABELA.
           10  37-CPRODT                   PIC S9(3)V COMP-3.
           10  37-DINIC-PARM-CONFG         PIC X(10).
           10  37-CEMPR-GSTOR-OPER         PIC S9(5)V COMP-3.
           10  37-CDEPDC-GSTOR-OPER        PIC S9(5)V COMP-3.
           10  37-CEMPR-GSTOR-NEGOC        PIC S9(5)V COMP-3.
           10  37-CDEPDC-GSTOR-NEGOC       PIC S9(5)V COMP-3.
           10  37-CMOEDA-OFIC-DESC         PIC S9(5)V COMP-3.
           10  37-HINIC-TRANS-ONLINE       PIC X(8).
           10  37-HFIM-TRANS-ONLINE        PIC X(8).
           10  37-HLIM-OPER-NORML          PIC X(8).
           10  37-HLIM-OPER-RETRO          PIC X(8).
           10  37-TMAX-OPER-ATRSO          PIC S9(3)V COMP-3.
           10  37-CINDCD-PRZ-OPER          PIC X(2).
           10  37-DFCHTO-MES               PIC X(10).
           10  37-CSIT-ACSSO-ONLINE        PIC X(1).
           10  37-HSIT-ACSSO-ONLINE        PIC X(26).
           10  37-HULT-ATULZ               PIC X(26).
           10  37-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  37-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  37-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  37-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  37-RETORNO.
       05  37-COD-RETORNO                         PIC 9(04).
       05  37-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  37-PROXIMO-RESTART                     PIC 9(05).
       05  37-QTDE-RETORNADA                      PIC 9(03).
       05  37-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  37-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T37.
           10  37RT-CPRODT                 PIC S9(3)V COMP-3.
           10  37RT-DINIC-PARM-CONFG       PIC X(10).
           10  37RT-CEMPR-GSTOR-OPER       PIC S9(5)V COMP-3.
           10  37RT-CDEPDC-GSTOR-OPER      PIC S9(5)V COMP-3.
           10  37RT-CEMPR-GSTOR-NEGOC      PIC S9(5)V COMP-3.
           10  37RT-CDEPDC-GSTOR-NEGOC     PIC S9(5)V COMP-3.
           10  37RT-CMOEDA-OFIC-DESC       PIC S9(5)V COMP-3.
           10  37RT-HINIC-TRANS-ONLINE     PIC X(8).
           10  37RT-HFIM-TRANS-ONLINE      PIC X(8).
           10  37RT-HLIM-OPER-NORML        PIC X(8).
           10  37RT-HLIM-OPER-RETRO        PIC X(8).
           10  37RT-TMAX-OPER-ATRSO        PIC S9(3)V COMP-3.
           10  37RT-CINDCD-PRZ-OPER        PIC X(2).
           10  37RT-DFCHTO-MES             PIC X(10).
           10  37RT-CSIT-ACSSO-ONLINE      PIC X(1).
           10  37RT-HSIT-ACSSO-ONLINE      PIC X(26).
           10  37RT-HULT-ATULZ             PIC X(26).
           10  37RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  37RT-CTERM                  PIC X(8).
