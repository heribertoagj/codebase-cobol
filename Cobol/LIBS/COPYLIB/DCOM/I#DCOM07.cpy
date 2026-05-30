      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  07-ARGUMENTOS-ENTRADA.

       05  07-COLUNAS-DA-TABELA.
           10  07-CCNPJ-CPF                PIC S9(9)V COMP-3.
           10  07-NSEQ-PSSOA-DESC          PIC S9(3)V COMP-3.
           10  07-IAVALS-DESC-COML         PIC X(40).
           10  07-CINDCD-AVALS-CLI         PIC X(1).
           10  07-CFLIAL-CNPJ              PIC S9(5)V COMP-3.
           10  07-CCTRL-CNPJ-CPF           PIC S9(2)V COMP-3.
           10  07-CCLUB                    PIC S9(10)V COMP-3.
           10  07-CBCO                     PIC S9(3)V COMP-3.
           10  07-CAG-BCRIA                PIC S9(5)V COMP-3.
           10  07-CCTA-BCRIA-CLI           PIC S9(13)V COMP-3.
           10  07-HULT-ATULZ               PIC X(26).
           10  07-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  07-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  07-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  07-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  07-RETORNO.
       05  07-COD-RETORNO                         PIC 9(04).
       05  07-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  07-PROXIMO-RESTART                     PIC 9(05).
       05  07-QTDE-RETORNADA                      PIC 9(03).
       05  07-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  07-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T07.
           10  07RT-CCNPJ-CPF              PIC S9(9)V COMP-3.
           10  07RT-NSEQ-PSSOA-DESC        PIC S9(3)V COMP-3.
           10  07RT-IAVALS-DESC-COML       PIC X(40).
           10  07RT-CINDCD-AVALS-CLI       PIC X(1).
           10  07RT-CFLIAL-CNPJ            PIC S9(5)V COMP-3.
           10  07RT-CCTRL-CNPJ-CPF         PIC S9(2)V COMP-3.
           10  07RT-CCLUB                  PIC S9(10)V COMP-3.
           10  07RT-CBCO                   PIC S9(3)V COMP-3.
           10  07RT-CAG-BCRIA              PIC S9(5)V COMP-3.
           10  07RT-CCTA-BCRIA-CLI         PIC S9(13)V COMP-3.
           10  07RT-HULT-ATULZ             PIC X(26).
           10  07RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  07RT-CTERM                  PIC X(8).
