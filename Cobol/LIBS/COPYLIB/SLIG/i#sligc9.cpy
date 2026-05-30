      ***===========================================================***
      *** NOME INC                                     LENGTH=0200  ***
      *** I#SLIGC9                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO MOVTO, CHEQUES REAPRESENTADOS ANTER.    **
      **             DEVOLVIDOS                                      **
      **                                                             **
      ***===========================================================***
      *DATA        PROJETO                                 ANALISTA    *
      *01/08/2025  CNPJ ALFANUMERICO                       LUCIANA SATO*
      *----------------------------------------------------------------*
       01 SLIGC9-REG-CHE-CLI.
          03 SLIGC9-CONTROLE.
CNPJ7C       05 SLIGC9-CGC-CPF-CLI             PIC X(009).
             05 SLIGC9-NOME-CLI                PIC X(040).
             05 SLIGC9-PRODUTO                 PIC X(007).
             05 SLIGC9-TIPO-CARTEIRA           PIC 9(001).
          03 SLIGC9-PERIODOS.
             05  SLIGC9-DT-PER-INI             PIC 9(008).
             05  SLIGC9-DT-PER-FIM             PIC 9(008).
          03 SLIGC9-VALORES.
             05 SLIGC9-VL-TOT-CART             PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-VCT-CART             PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-BXA-ANTEC            PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-DEP-CHE-PER          PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-PAGOS            PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-PRI-DEV          PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-REAPRES          PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-REAPR-PAG        PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-REAPR-DEV        PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-ANT-REAP-DEV     PIC 9(015)V99 COMP-3.
             05 SLIGC9-VL-CHE-DEV-PER-ANT      PIC 9(015)V99 COMP-3.
             05 FILLER                         PIC X(028).
