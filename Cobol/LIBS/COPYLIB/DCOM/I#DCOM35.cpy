      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  35-ARGUMENTOS-ENTRADA.

       05  35-COLUNAS-DA-TABELA.
           10  35-CCONVE-CLI-DESC          PIC S9(9)V COMP-3.
           10  35-CVRSAO-CONVE-CLI         PIC S9(3)V COMP-3.
           10  35-CINDCD-VCTO-PCELA        PIC X(1).
           10  35-CMEIO-PGTO-DESC          PIC S9(3)V COMP-3.
           10  35-CINDCD-RESP-PGTO         PIC X(1).
           10  35-TDEB-PCELA-VENCD         PIC S9(3)V COMP-3.
           10  35-CINDCD-PRZ-PCELA         PIC X(2).
           10  35-HULT-ATULZ               PIC X(26).
           10  35-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  35-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  35-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  35-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  35-RETORNO.
       05  35-COD-RETORNO                         PIC 9(04).
       05  35-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  35-PROXIMO-RESTART                     PIC 9(05).
       05  35-QTDE-RETORNADA                      PIC 9(03).
       05  35-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  35-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T35.
           10  35RT-CCONVE-CLI-DESC        PIC S9(9)V COMP-3.
           10  35RT-CVRSAO-CONVE-CLI       PIC S9(3)V COMP-3.
           10  35RT-CINDCD-VCTO-PCELA      PIC X(1).
           10  35RT-CMEIO-PGTO-DESC        PIC S9(3)V COMP-3.
           10  35RT-CINDCD-RESP-PGTO       PIC X(1).
           10  35RT-TDEB-PCELA-VENCD       PIC S9(3)V COMP-3.
           10  35RT-CINDCD-PRZ-PCELA       PIC X(2).
           10  35RT-HULT-ATULZ             PIC X(26).
           10  35RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  35RT-CTERM                  PIC X(8).
