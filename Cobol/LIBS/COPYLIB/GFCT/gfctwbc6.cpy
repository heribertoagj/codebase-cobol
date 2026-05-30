      *
      *****************************************************************
      * NOME DA INC - GFCTWBC6                                        *
      * DESCRICAO   - TARIFAS - CADASTRO DE PROCESSOS MASSIVOS        *
      *               IMAGEM DA BASE GFCTB0N8 - CONTROLE              *
      * TAMANHO     - 164                                             *
      * DATA        - MAR/2013                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *****************************************************************
      *
       01  GFCTWBC6-REG.
           03 BC6-HINCL-REG                      PIC X(26).
           03 BC6-CTPO-OPER-MASSV                PIC S9(1)V COMP-3.
           03 BC6-DPROCM-AGNDA                   PIC X(10).
           03 BC6-CSIT-PROCM-MASSV               PIC S9(1)V COMP-3.
           03 BC6-ROBS-PROCM-MASSV               PIC X(80).
           03 BC6-QREG-LIDO-MASSV                PIC S9(9)V COMP-3.
           03 BC6-QREG-ACTAC-MASSV               PIC S9(9)V COMP-3.
           03 BC6-QREG-REJTD-MASSV               PIC S9(9)V COMP-3.
           03 BC6-CFUNC-MANUT                    PIC S9(9)V COMP-3.
           03 BC6-HMANUT-REG                     PIC X(26).
      *
      *****************************************************************
      * FIM DA INC - GFCTWBC6                                         *
      *****************************************************************
