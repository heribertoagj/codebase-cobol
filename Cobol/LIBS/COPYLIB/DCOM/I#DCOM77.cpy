      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  77-ARGUMENTOS-ENTRADA.

       05  77-COLUNAS-DA-TABELA.
           10  77-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  77-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  77-ITESTM-DESC-COML         PIC X(40).
           10  77-CDOCTO-ID                PIC X(15).
           10  77-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  77-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  77-HULT-ATULZ               PIC X(26).
           10  77-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  77-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  77-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  77-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  77-RETORNO.
       05  77-COD-RETORNO                         PIC 9(04).
       05  77-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  77-PROXIMO-RESTART                     PIC 9(05).
       05  77-QTDE-RETORNADA                      PIC 9(03).
       05  77-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  77-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T77.
           10  77RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  77RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  77RT-ITESTM-DESC-COML       PIC X(40).
           10  77RT-CDOCTO-ID              PIC X(15).
           10  77RT-CFLIAL-CNPJ            PIC S9(5)V COMP-3.
           10  77RT-CCTRL-CNPJ-CPF         PIC S9(2)V COMP-3.
           10  77RT-HULT-ATULZ             PIC X(26).
           10  77RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  77RT-CTERM                  PIC X(8).
