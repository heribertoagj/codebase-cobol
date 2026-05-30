
      ***===========================================================***
      *** NOME INC                                     LENGTH=070   ***
      *** I#SLIG63                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO IMAGEM DA TABELA                       ***
      ***            SOLICITACAO RELATORIO LIQUIDEZ                 ***
      ***===========================================================***
      *
       01  REG-SOLCT-RELAT-LIQDZ.
           05 SLIG63-CCGC-CPF-CLI           PIC S9(09) COMP-3.
           05 SLIG63-CINDCD-PSSOA-CLI       PIC X(001).
           05 SLIG63-CIDTFD-PRODT-LIQDZ     PIC S9(02) COMP-3.
           05 SLIG63-DT-PER-INI             PIC X(010).
           05 SLIG63-DT-PER-FIM             PIC X(010).
           05 SLIG63-UCUSAR-SENHA           PIC X(007).
           05 SLIG63-CTERM                  PIC X(008).
           05 SLIG63-CTERM-NULL             PIC X(001).
           05 SLIG63-HATULZ                 PIC X(026).

      * === CAMPOS DUPLICADOS PARA STRING ===
           05 SLIG63-CCGC-CPF-CLI-ST        PIC X(009).
