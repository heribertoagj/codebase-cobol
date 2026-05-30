      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D7 - DB2PRD.TSACDO_TITLO_OPER      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D7-ARGUMENTOS-ENTRADA.

       05  D7-COLUNAS-DA-TABELA.
           10  D7-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  D7-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  D7-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  D7-IPSSOA-SACDO                 PIC X(60).
           10  D7-CINDCD-PSSOA-CRRTT           PIC X(1).
           10  D7-CCEP                         PIC S9(5)V COMP-3.
           10  D7-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  D7-ELOGDR                       PIC X(40).
           10  D7-IBAIRO                       PIC X(40).
           10  D7-IMUN                         PIC X(30).
           10  D7-CSGL-UF                      PIC X(2).
           10  D7-HATULZ-ANLSE-SACDO           PIC X(26).
           10  D7-CCLUB                        PIC S9(10)V COMP-3.
           10  D7-DINCL-REG                    PIC X(10).
           10  D7-HULT-ATULZ                   PIC X(26).
           10  D7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  D7-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D7-RETORNO.
       05  D7-COD-RETORNO                         PIC 9(04).
       05  D7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D7-PROXIMO-RESTART                     PIC 9(05).
       05  D7-QTDE-RETORNADA                      PIC 9(03).
       05  D7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD7.
           10  D7RT-CCNPJ-CPF                  PIC S9(9)V COMP-3.
           10  D7RT-CFLIAL-CNPJ                PIC S9(5)V COMP-3.
           10  D7RT-CCTRL-CNPJ-CPF             PIC S9(2)V COMP-3.
           10  D7RT-IPSSOA-SACDO               PIC X(60).
           10  D7RT-CINDCD-PSSOA-CRRTT         PIC X(1).
           10  D7RT-CCEP                       PIC S9(5)V COMP-3.
           10  D7RT-CCEP-COMPL                 PIC S9(3)V COMP-3.
           10  D7RT-ELOGDR                     PIC X(40).
           10  D7RT-IBAIRO                     PIC X(40).
           10  D7RT-IMUN                       PIC X(30).
           10  D7RT-CSGL-UF                    PIC X(2).
           10  D7RT-HATULZ-ANLSE-SACDO         PIC X(26).
           10  D7RT-CCLUB                      PIC S9(10)V COMP-3.
           10  D7RT-DINCL-REG                  PIC X(10).
           10  D7RT-HULT-ATULZ                 PIC X(26).
           10  D7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  D7RT-CTERM                      PIC X(8).
