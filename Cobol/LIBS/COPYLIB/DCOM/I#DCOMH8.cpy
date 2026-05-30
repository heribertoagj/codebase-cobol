      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H8 - DB2PRD.TTRASI_MEIO_LIBRC      *
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

       01  H8-ARGUMENTOS-ENTRADA.

       05  H8-COLUNAS-DA-TABELA.
           10  H8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  H8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  H8-CMEIO-LIBRC-DESC             PIC S9(3)V COMP-3.
           10  H8-NOPER-MEIO-LIBRC             PIC S9(3)V COMP-3.
           10  H8-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  H8-HSIT-DESC-COML               PIC X(26).
           10  H8-CBCO                         PIC S9(3)V COMP-3.
           10  H8-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  H8-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  H8-CPOSTO-SERVC                 PIC S9(3)V COMP-3.
           10  H8-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  H8-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  H8-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  H8-IPSSOA-DESC-COML             PIC X(60).
           10  H8-CCLUB                        PIC S9(10)V COMP-3.
           10  H8-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  H8-PLIBRC-DESC-COML             PIC S9(3)V9(3) COMP-3.
           10  H8-DLIBRC-DESC-COML             PIC X(10).
           10  H8-CINDCD-LIBRC-ANTCP           PIC X(1).
           10  H8-HULT-ATULZ                   PIC X(26).
           10  H8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H8-CTERM                        PIC X(8).
BRQ001     10  H8-CINDCD-LIBRC-CANAL           PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H8-RETORNO.
       05  H8-COD-RETORNO                         PIC 9(04).
       05  H8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H8-PROXIMO-RESTART                     PIC 9(05).
       05  H8-QTDE-RETORNADA                      PIC 9(03).
       05  H8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH8.
           10  H8RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  H8RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  H8RT-CMEIO-LIBRC-DESC           PIC S9(3)V COMP-3.
           10  H8RT-NOPER-MEIO-LIBRC           PIC S9(3)V COMP-3.
           10  H8RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  H8RT-HSIT-DESC-COML             PIC X(26).
           10  H8RT-CBCO                       PIC S9(3)V COMP-3.
           10  H8RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  H8RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  H8RT-CPOSTO-SERVC               PIC S9(3)V COMP-3.
           10  H8RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  H8RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  H8RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  H8RT-IPSSOA-DESC-COML           PIC X(60).
           10  H8RT-CCLUB                      PIC S9(10)V COMP-3.
           10  H8RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  H8RT-PLIBRC-DESC-COML           PIC S9(3)V9(3) COMP-3.
           10  H8RT-DLIBRC-DESC-COML           PIC X(10).
           10  H8RT-CINDCD-LIBRC-ANTCP         PIC X(1).
           10  H8RT-HULT-ATULZ                 PIC X(26).
           10  H8RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H8RT-CTERM                      PIC X(8).
BRQ001     10  H8RT-CINDCD-LIBRC-CANAL         PIC X(1).
