      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0C7 - DB2PRD.TOPER_MEIO_LIBRC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
BRQ001* ALTERACAO  -  BRQ DIGITAL SOLUTIONS                           *
BRQ001*---------------------------------------------------------------*
BRQ001* DATA....:  22/11/2018                                         *
BRQ001* OBJETIVO:  INCLUIR INDICADOR DE LIBERACAO NOS CANAIS          *
BRQ001* PROJETO.:  BRQ - 18/0456 - BORDERO ELETRONICO                 *
BRQ001*===============================================================*

       01  C7-ARGUMENTOS-ENTRADA.

       05  C7-COLUNAS-DA-TABELA.
           10  C7-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  C7-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  C7-CMEIO-LIBRC-DESC             PIC S9(3)V COMP-3.
           10  C7-NOPER-MEIO-LIBRC             PIC S9(3)V COMP-3.
           10  C7-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  C7-HSIT-DESC-COML               PIC X(26).
           10  C7-CBCO                         PIC S9(3)V COMP-3.
           10  C7-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  C7-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  C7-CPOSTO-SERVC                 PIC S9(3)V COMP-3.
           10  C7-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  C7-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  C7-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  C7-IPSSOA-DESC-COML             PIC X(60).
           10  C7-CCLUB                        PIC S9(10)V COMP-3.
           10  C7-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  C7-PLIBRC-DESC-COML             PIC S9(3)V9(3) COMP-3.
           10  C7-DLIBRC-DESC-COML             PIC X(10).
           10  C7-CINDCD-LIBRC-ANTCP           PIC X(1).
           10  C7-HULT-ATULZ                   PIC X(26).
           10  C7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  C7-CTERM                        PIC X(8).
BRQ001     10  C7-CINDCD-LIBRC-CANAL           PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  C7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  C7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  C7-RETORNO.
       05  C7-COD-RETORNO                         PIC 9(04).
       05  C7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  C7-PROXIMO-RESTART                     PIC 9(05).
       05  C7-QTDE-RETORNADA                      PIC 9(03).
       05  C7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  C7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TC7.
           10  C7RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  C7RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  C7RT-CMEIO-LIBRC-DESC           PIC S9(3)V COMP-3.
           10  C7RT-NOPER-MEIO-LIBRC           PIC S9(3)V COMP-3.
           10  C7RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  C7RT-HSIT-DESC-COML             PIC X(26).
           10  C7RT-CBCO                       PIC S9(3)V COMP-3.
           10  C7RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  C7RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  C7RT-CPOSTO-SERVC               PIC S9(3)V COMP-3.
           10  C7RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  C7RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  C7RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  C7RT-IPSSOA-DESC-COML           PIC X(60).
           10  C7RT-CCLUB                      PIC S9(10)V COMP-3.
           10  C7RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  C7RT-PLIBRC-DESC-COML           PIC S9(3)V9(3) COMP-3.
           10  C7RT-DLIBRC-DESC-COML           PIC X(10).
           10  C7RT-CINDCD-LIBRC-ANTCP         PIC X(1).
           10  C7RT-HULT-ATULZ                 PIC X(26).
           10  C7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  C7RT-CTERM                      PIC X(8).
BRQ001     10  C7RT-CINDCD-LIBRC-CANAL         PIC X(1).
