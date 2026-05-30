      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0B4 - DB2PRD.THIST_MEIO_LIBRC       *
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

       01  B4-ARGUMENTOS-ENTRADA.

       05  B4-COLUNAS-DA-TABELA.
           10  B4-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  B4-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  B4-CMEIO-LIBRC-DESC             PIC S9(3)V COMP-3.
           10  B4-NOPER-MEIO-LIBRC             PIC S9(3)V COMP-3.
           10  B4-HULT-ATULZ                   PIC X(26).
           10  B4-CSIT-DESC-COML               PIC S9(3)V COMP-3.
           10  B4-HSIT-DESC-COML               PIC X(26).
           10  B4-CBCO                         PIC S9(3)V COMP-3.
           10  B4-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  B4-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  B4-CPOSTO-SERVC                 PIC S9(3)V COMP-3.
           10  B4-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  B4-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  B4-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  B4-IPSSOA-DESC-COML             PIC X(60).
           10  B4-CCLUB                        PIC S9(10)V COMP-3.
           10  B4-VLIBRC-DESC-COML             PIC S9(15)V9(2) COMP-3.
           10  B4-PLIBRC-DESC-COML             PIC S9(3)V9(3) COMP-3.
           10  B4-DLIBRC-DESC-COML             PIC X(10).
           10  B4-CINDCD-LIBRC-ANTCP           PIC X(1).
           10  B4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  B4-CTERM                        PIC X(8).
BRQ001     10  B4-CINDCD-LIBRC-CANAL           PIC X(1).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  B4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  B4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  B4-RETORNO.
       05  B4-COD-RETORNO                         PIC 9(04).
       05  B4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  B4-PROXIMO-RESTART                     PIC 9(05).
       05  B4-QTDE-RETORNADA                      PIC 9(03).
       05  B4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  B4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TB4.
           10  B4RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  B4RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  B4RT-CMEIO-LIBRC-DESC           PIC S9(3)V COMP-3.
           10  B4RT-NOPER-MEIO-LIBRC           PIC S9(3)V COMP-3.
           10  B4RT-HULT-ATULZ                 PIC X(26).
           10  B4RT-CSIT-DESC-COML             PIC S9(3)V COMP-3.
           10  B4RT-HSIT-DESC-COML             PIC X(26).
           10  B4RT-CBCO                       PIC S9(3)V COMP-3.
           10  B4RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  B4RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  B4RT-CPOSTO-SERVC               PIC S9(3)V COMP-3.
           10  B4RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  B4RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  B4RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  B4RT-IPSSOA-DESC-COML           PIC X(60).
           10  B4RT-CCLUB                      PIC S9(10)V COMP-3.
           10  B4RT-VLIBRC-DESC-COML           PIC S9(15)V9(2) COMP-3.
           10  B4RT-PLIBRC-DESC-COML           PIC S9(3)V9(3) COMP-3.
           10  B4RT-DLIBRC-DESC-COML           PIC X(10).
           10  B4RT-CINDCD-LIBRC-ANTCP         PIC X(1).
           10  B4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  B4RT-CTERM                      PIC X(8).
BRQ001     10  B4RT-CINDCD-LIBRC-CANAL         PIC X(1).
