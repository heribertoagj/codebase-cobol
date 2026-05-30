      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D6 - DB2PRD.TRELAC_PSSOA_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D6-ARGUMENTOS-ENTRADA.

       05  D6-COLUNAS-DA-TABELA.
           10  D6-NPSSOA-DESC-COML             PIC S9(9)V COMP-3.
           10  D6-NRELAC-PSSOA-DESC            PIC S9(9)V COMP-3.
           10  D6-CINDCD-PSSOA-DESC            PIC X(2).
           10  D6-CELMTO-DESC-COML             PIC S9(3)V COMP-3.
           10  D6-CCHAVE-ELMTO-DESC            PIC X(45).
           10  D6-CBCO                         PIC S9(3)V COMP-3.
           10  D6-CAG-BCRIA                    PIC S9(5)V COMP-3.
           10  D6-CCTA-BCRIA-CLI               PIC S9(13)V COMP-3.
           10  D6-CPOSTO-SERVC                 PIC S9(3)V COMP-3.
           10  D6-IPSSOA-CONJG                 PIC X(40).
           10  D6-CCPF-CONJG                   PIC S9(9)V COMP-3.
           10  D6-CCTRL-CPF-CONJG              PIC S9(2)V COMP-3.
           10  D6-CDOCTO-ID-CONJG              PIC X(15).
           10  D6-CINDCD-CONJG-AUTRZ           PIC X(1).
           10  D6-CCEP                         PIC S9(5)V COMP-3.
           10  D6-CCEP-COMPL                   PIC S9(3)V COMP-3.
           10  D6-ELOGDR                       PIC X(40).
           10  D6-CSGL-UF                      PIC X(2).
           10  D6-DINCL-REG                    PIC X(10).
           10  D6-HULT-ATULZ                   PIC X(26).
           10  D6-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  D6-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D6-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D6-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D6-RETORNO.
       05  D6-COD-RETORNO                         PIC 9(04).
       05  D6-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D6-PROXIMO-RESTART                     PIC 9(05).
       05  D6-QTDE-RETORNADA                      PIC 9(03).
       05  D6-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D6-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD6.
           10  D6RT-NPSSOA-DESC-COML           PIC S9(9)V COMP-3.
           10  D6RT-NRELAC-PSSOA-DESC          PIC S9(9)V COMP-3.
           10  D6RT-CINDCD-PSSOA-DESC          PIC X(2).
           10  D6RT-CELMTO-DESC-COML           PIC S9(3)V COMP-3.
           10  D6RT-CCHAVE-ELMTO-DESC          PIC X(45).
           10  D6RT-CBCO                       PIC S9(3)V COMP-3.
           10  D6RT-CAG-BCRIA                  PIC S9(5)V COMP-3.
           10  D6RT-CCTA-BCRIA-CLI             PIC S9(13)V COMP-3.
           10  D6RT-CPOSTO-SERVC               PIC S9(3)V COMP-3.
           10  D6RT-IPSSOA-CONJG               PIC X(40).
           10  D6RT-CCPF-CONJG                 PIC S9(9)V COMP-3.
           10  D6RT-CCTRL-CPF-CONJG            PIC S9(2)V COMP-3.
           10  D6RT-CDOCTO-ID-CONJG            PIC X(15).
           10  D6RT-CINDCD-CONJG-AUTRZ         PIC X(1).
           10  D6RT-CCEP                       PIC S9(5)V COMP-3.
           10  D6RT-CCEP-COMPL                 PIC S9(3)V COMP-3.
           10  D6RT-ELOGDR                     PIC X(40).
           10  D6RT-CSGL-UF                    PIC X(2).
           10  D6RT-DINCL-REG                  PIC X(10).
           10  D6RT-HULT-ATULZ                 PIC X(26).
           10  D6RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  D6RT-CTERM                      PIC X(8).
