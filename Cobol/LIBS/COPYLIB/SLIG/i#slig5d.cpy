      ***===========================================================***
      *** NOME INC                                     LENGTH=0308  ***
      *** I#SLIG5D                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS TITULOS              ***
      ***                                                           ***
      ***===========================================================***
       01  REG-SELECIONADO.
           03  SLIG5D-CTRL.
               05 SLIG5D-CGC-CPF-CTRL            PIC X(009).
               05 SLIG5D-PRODUTO-CTRL            PIC X(001).
               05 SLIG5D-CARTEIRA-CTRL           PIC 9(001).
               05 SLIG5D-CD-PRODUTO-CTRL         PIC 9(002).
               05 SLIG5D-CD-CNEGOC-CTRL          PIC 9(011).
               05 SLIG5D-CD-NOSNRO-CTRL          PIC 9(011).
               05 SLIG5D-DT-ENTRADA-SISTEMA-CTRL PIC 9(008).
               05 SLIG5D-DT-VENCTO-PROD-CTRL     PIC 9(008).
           03  SLIG5D-IDENT-DOCTO.
               05  SLIG5D-CD-PRODUTO-CLI         PIC 9(002).
               05  FILLER                        PIC X(002).
               05  SLIG5D-AGENCIA-CLI            PIC 9(004).
               05  FILLER                        PIC X(002).
               05  SLIG5D-CONTA-CLI              PIC 9(007).
               05  FILLER                        PIC X(002).
               05  SLIG5D-NUM-TIT                PIC 9(011).
               05  FILLER                        PIC X(005).
           03  SLIG5D-PERIODO.
               05  SLIG5D-DT-PER-INI             PIC 9(008).
               05  SLIG5D-DT-PER-FIM             PIC 9(008).
           03  SLIG5D-REG-TITULO.
               05 SLIG5D-BCO-CLIENTE             PIC 9(003) COMP-3.
               05 SLIG5D-AGENCIA-CLIENTE         PIC 9(005) COMP-3.
               05 SLIG5D-CONTA-CLIENTE           PIC 9(013) COMP-3.
               05 SLIG5D-CTPO-CART               PIC 9(003) COMP-3.
               05 SLIG5D-CCLI-COBR               PIC 9(009) COMP-3.
               05 SLIG5D-CGC-CPF-CLIENTE         PIC 9(009) COMP-3.
               05 SLIG5D-FILIAL-CLIENTE          PIC 9(005) COMP-3.
               05 SLIG5D-CONTROLE-CLIENTE        PIC 9(002).
               05 SLIG5D-BCO-SACADO              PIC 9(003) COMP-3.
               05 SLIG5D-AGENCIA-SACADO          PIC 9(005) COMP-3.
               05 SLIG5D-CONTA-SACADO            PIC 9(013) COMP-3.
               05 SLIG5D-CGC-CPF-SACADO          PIC 9(009) COMP-3.
               05 SLIG5D-FILIAL-SACADO           PIC 9(005) COMP-3.
               05 SLIG5D-CONTROLE-SACADO         PIC 9(002).
               05 SLIG5D-VL-PRINC-PROD           PIC 9(017) COMP-3.
               05 SLIG5D-VL-PAGO-PROD            PIC 9(015) COMP-3.
               05 SLIG5D-VL-DESC-ABAT-PROD       PIC 9(015) COMP-3.
               05 SLIG5D-DT-VENCTO-PROD          PIC 9(009) COMP-3.
               05 SLIG5D-DT-PAGTO-PROD           PIC 9(009) COMP-3.
               05 SLIG5D-DT-ENTRADA-ORIGEM       PIC 9(009) COMP-3.
               05 SLIG5D-DT-ENTRADA-SISTEMA      PIC 9(009) COMP-3.
               05 SLIG5D-DT-VENCTO-ANTERIOR      PIC 9(009) COMP-3.
               05 SLIG5D-CD-PRODUTO              PIC 9(002).
               05 SLIG5D-CD-CNEGOC               PIC 9(011) COMP-3.
               05 SLIG5D-CD-NOSNRO               PIC 9(011) COMP-3.
               05 SLIG5D-CD-SEQUENCIA            PIC 9(003) COMP-3.
               05 SLIG5D-CD-COD-NTAX             PIC 9(005) COMP-3.
               05 SLIG5D-CD-OCO-REM-L            PIC 9(003) COMP-3.
               05 SLIG5D-CD-MOT-REM-SIS          PIC 9(003) COMP-3.
               05 SLIG5D-CD-ESPECIE              PIC 9(003) COMP-3.
               05 SLIG5D-CD-ORIGEM               PIC 9(003) COMP-3.
               05 SLIG5D-DT-INICIO-PER           PIC 9(009) COMP-3.
               05 SLIG5D-DT-FINAL-PER            PIC 9(009) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
               05 SLIG5D-CGC-CPF-CLIENTE-ST      PIC X(009).
               05 SLIG5D-FILIAL-CLIENTE-ST       PIC X(005).
               05 SLIG5D-CONTROLE-CLIENTE-ST     PIC 9(002).
               05 SLIG5D-CGC-CPF-SACADO-ST       PIC X(009).
               05 SLIG5D-FILIAL-SACADO-ST        PIC X(005).
               05 SLIG5D-CONTROLE-SACADO-ST      PIC 9(002).
               05 FILLER                         PIC X(036).
