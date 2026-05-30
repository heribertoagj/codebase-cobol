      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I8 - DB2PRD.THIST_PSSOA_OPER       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I8-ARGUMENTOS-ENTRADA.

       05  I8-COLUNAS-DA-TABELA.
           10  I8-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  I8-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  I8-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  I8-HULT-ATULZ                   PIC X(26).
           10  I8-NPSSOA-DESC-COML             PIC S9(9)V COMP-3.
           10  I8-NRELAC-PSSOA-DESC            PIC S9(9)V COMP-3.
           10  I8-CCNPJ-CPF                    PIC S9(9)V COMP-3.
           10  I8-CFLIAL-CNPJ                  PIC S9(5)V COMP-3.
           10  I8-CCTRL-CNPJ-CPF               PIC S9(2)V COMP-3.
           10  I8-CDOCTO-ID                    PIC X(15).
           10  I8-IPSSOA-DESC-COML             PIC X(60).
           10  I8-CCLUB                        PIC S9(10)V COMP-3.
           10  I8-CINDCD-PSSOA-DESC            PIC X(2).
           10  I8-CBCO                         PIC S9(3)V COMP-3.
           10  I8-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  I8-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  I8-CPOSTO-SERVC                 PIC S9(5)V COMP-3.
           10  I8-IPSSOA-CONJG                 PIC X(40).
           10  I8-CCPF-CONJG                   PIC S9(9)V COMP-3.
           10  I8-CCTRL-CPF-CONJG              PIC S9(2)V COMP-3.
           10  I8-CDOCTO-ID-CONJG              PIC X(15).
           10  I8-CINDCD-CONJG-AUTRZ           PIC X(1).
           10  I8-CCEP                         PIC S9(5)V COMP-3.
           10  I8-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  I8-ELOGDR                       PIC X(40).
           10  I8-CSGL-UF                      PIC X(2).
           10  I8-DINCL-REG                    PIC X(10).
           10  I8-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  I8-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I8-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I8-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I8-RETORNO.
       05  I8-COD-RETORNO                         PIC 9(04).
       05  I8-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I8-PROXIMO-RESTART                     PIC 9(05).
       05  I8-QTDE-RETORNADA                      PIC 9(03).
       05  I8-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I8-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI8.
           10  I8RT-DANO-OPER-DESC              PIC S9(4)V COMP-3.
           10  I8RT-NSEQ-OPER-DESC              PIC S9(9)V COMP-3.
           10  I8RT-NPCELA-DESC-COML            PIC S9(5)V COMP-3.
           10  I8RT-HULT-ATULZ                  PIC X(26).
           10  I8RT-NPSSOA-DESC-COML            PIC S9(9)V COMP-3.
           10  I8RT-NRELAC-PSSOA-DESC           PIC S9(9)V COMP-3.
           10  I8RT-CCNPJ-CPF                   PIC S9(9)V COMP-3.
           10  I8RT-CFLIAL-CNPJ                 PIC S9(5)V COMP-3.
           10  I8RT-CCTRL-CNPJ-CPF              PIC S9(2)V COMP-3.
           10  I8RT-CDOCTO-ID                   PIC X(15).
           10  I8RT-IPSSOA-DESC-COML            PIC X(60).
           10  I8RT-CCLUB                       PIC S9(10)V COMP-3.
           10  I8RT-CINDCD-PSSOA-DESC           PIC X(2).
           10  I8RT-CBCO                        PIC S9(3)V COMP-3.
           10  I8RT-CAG-BCRIA                   PIC S9(5)V COMP-3.
           10  I8RT-CCTA-BCRIA-CLI              PIC S9(13)V COMP-3.
           10  I8RT-CPOSTO-SERVC                PIC S9(5)V COMP-3.
           10  I8RT-IPSSOA-CONJG                PIC X(40).
           10  I8RT-CCPF-CONJG                  PIC S9(9)V COMP-3.
           10  I8RT-CCTRL-CPF-CONJG             PIC S9(2)V COMP-3.
           10  I8RT-CDOCTO-ID-CONJG             PIC X(15).
           10  I8RT-CINDCD-CONJG-AUTRZ          PIC X(1).
           10  I8RT-CCEP                        PIC S9(5)V COMP-3.
           10  I8RT-CCEP-COMPL                  PIC S9(3)V COMP-3.
           10  I8RT-ELOGDR                      PIC X(40).
           10  I8RT-CSGL-UF                     PIC X(2).
           10  I8RT-DINCL-REG                   PIC X(10).
           10  I8RT-CFUNC-BDSCO                 PIC S9(9)V COMP-3.
           10  I8RT-CTERM                       PIC X(8).
