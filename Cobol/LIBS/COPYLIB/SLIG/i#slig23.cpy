      ***===========================================================***
      *** NOME INC                                     LENGTH=00142 ***
      *** I#SLIG23                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: ARQUIVO DE PRORROGACOES                        ***
      ***                                                           ***
      ***===========================================================***

       01  REG-PRORROGE.
           05  SLIG23-CHAVE-E.
             07 SLIG23-CCGC-CLIENTE-E-ST.
              09 SLIG23-CCGC-CLIENTE-E       PIC 9(009).
             07 SLIG23-FILIAL-CLIENTE-E-ST.
              09 SLIG23-FILIAL-CLIENTE-E     PIC 9(005).
             07 SLIG23-CCGC-SACADO-E-ST.
              09 SLIG23-CCGC-SACADO-E        PIC 9(009).
             07 SLIG23-FILIAL-SACADO-E-ST.
              09 SLIG23-FILIAL-SACADO-E      PIC 9(005).
             07 SLIG23-CONTROLE-SACADO-E     PIC 9(002).
             07 SLIG23-CTPO-CART-E           PIC 9(003).
           05  SLIG23-CCLI-E                 PIC 9(009) COMP-3.
           05  SLIG23-PRODUTO-E              PIC 9(002) COMP-3.
           05  SLIG23-CNEGOC-E               PIC 9(011) COMP-3.
           05  SLIG23-NOSNRO-E               PIC 9(011) COMP-3.
           05  SLIG23-SEQ-E                  PIC 9(003) COMP-3.
           05  SLIG23-DATVEN-E               PIC 9(009) COMP-3.
           05  SLIG23-VALOR-E                PIC 9(017) COMP-3.
           05  SLIG23-DATMOV-E               PIC 9(009) COMP-3.
           05  SLIG23-OCO-REM-E              PIC 9(003) COMP-3.
           05  SLIG23-MOT-REM-E              PIC 9(003) COMP-3.
           05  SLIG23-VENANT-E               PIC 9(009) COMP-3.
           05  SLIG23-DATPER-IN1-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-FI1-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-IN2-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-FI2-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-IN3-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-FI3-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-I1F-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-F1F-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-I2F-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-F2F-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-I3F-E           PIC 9(009) COMP-3.
           05  SLIG23-DATPER-F3F-E           PIC 9(009) COMP-3.
