      ***===========================================================***
      *** NOME INC                                     LENGTH=0308  ***
      *** I#SLIG54                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS TITULOS              ***
      ***                                                           ***
      ***===========================================================***
       01  REG-SELECIONADO.
           03  SLIG54-CTRL.
               05 SLIG54-CGC-CPF-CTRL-ST.
                  07 SLIG54-CGC-CPF-CTRL         PIC 9(009).
               05 SLIG54-PRODUTO-CTRL            PIC X(001).
               05 SLIG54-CARTEIRA-CTRL           PIC 9(001).
               05 SLIG54-CD-PRODUTO-CTRL         PIC 9(002).
               05 SLIG54-CD-CNEGOC-CTRL          PIC 9(011).
               05 SLIG54-CD-NOSNRO-CTRL          PIC 9(011).
               05 SLIG54-DT-ENTRADA-SISTEMA-CTRL PIC 9(008).
               05 SLIG54-DT-VENCTO-PROD-CTRL     PIC 9(008).
           03  SLIG54-IDENT-DOCTO.
               05  SLIG54-CD-PRODUTO-CLI         PIC 9(002).
               05  FILLER                        PIC X(002).
               05  SLIG54-AGENCIA-CLI            PIC 9(004).
               05  FILLER                        PIC X(002).
               05  SLIG54-CONTA-CLI              PIC 9(007).
               05  FILLER                        PIC X(002).
               05  SLIG54-NUM-TIT                PIC 9(011).
               05  FILLER                        PIC X(005).
           03  SLIG54-PERIODO.
               05  SLIG54-DT-PER-INI             PIC 9(008).
               05  SLIG54-DT-PER-FIM             PIC 9(008).
           03  SLIG54-REG-TITULO.
               05 SLIG54-BCO-CLIENTE             PIC 9(003) COMP-3.
               05 SLIG54-AGENCIA-CLIENTE         PIC 9(005) COMP-3.
               05 SLIG54-CONTA-CLIENTE           PIC 9(013) COMP-3.
               05 SLIG54-CTPO-CART               PIC 9(003) COMP-3.
               05 SLIG54-CCLI-COBR               PIC 9(009) COMP-3.
               05 SLIG54-CGC-CPF-CLIENTE         PIC 9(009) COMP-3.
               05 SLIG54-FILIAL-CLIENTE          PIC 9(005) COMP-3.
               05 SLIG54-CONTROLE-CLIENTE        PIC 9(002).
               05 SLIG54-BCO-SACADO              PIC 9(003) COMP-3.
               05 SLIG54-AGENCIA-SACADO          PIC 9(005) COMP-3.
               05 SLIG54-CONTA-SACADO            PIC 9(013) COMP-3.
               05 SLIG54-CGC-CPF-SACADO          PIC 9(009) COMP-3.
               05 SLIG54-FILIAL-SACADO           PIC 9(005) COMP-3.
               05 SLIG54-CONTROLE-SACADO         PIC 9(002).
               05 SLIG54-VL-PRINC-PROD           PIC 9(017) COMP-3.
               05 SLIG54-VL-PAGO-PROD            PIC 9(015) COMP-3.
               05 SLIG54-VL-DESC-ABAT-PROD       PIC 9(015) COMP-3.
               05 SLIG54-DT-VENCTO-PROD          PIC 9(009) COMP-3.
               05 SLIG54-DT-PAGTO-PROD           PIC 9(009) COMP-3.
               05 SLIG54-DT-ENTRADA-ORIGEM       PIC 9(009) COMP-3.
               05 SLIG54-DT-ENTRADA-SISTEMA      PIC 9(009) COMP-3.
               05 SLIG54-DT-VENCTO-ANTERIOR      PIC 9(009) COMP-3.
               05 SLIG54-CD-PRODUTO              PIC 9(002).
               05 SLIG54-CD-CNEGOC               PIC 9(011) COMP-3.
               05 SLIG54-CD-NOSNRO               PIC 9(011) COMP-3.
               05 SLIG54-CD-SEQUENCIA            PIC 9(003) COMP-3.
               05 SLIG54-CD-COD-NTAX             PIC 9(005) COMP-3.
               05 SLIG54-CD-OCO-REM-L            PIC 9(003) COMP-3.
               05 SLIG54-CD-MOT-REM-SIS          PIC 9(003) COMP-3.
               05 SLIG54-CD-ESPECIE              PIC 9(003) COMP-3.
               05 SLIG54-CD-ORIGEM               PIC 9(003) COMP-3.
               05 SLIG54-DT-INICIO-PER           PIC 9(009) COMP-3.
               05 SLIG54-DT-FINAL-PER            PIC 9(009) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
               05 SLIG54-CGC-CPF-CLIENTE-ST      PIC X(009).
               05 SLIG54-FILIAL-CLIENTE-ST       PIC X(005).
               05 SLIG54-CONTROLE-CLIENTE-ST     PIC 9(002).
               05 SLIG54-CGC-CPF-SACADO-ST       PIC X(009).
               05 SLIG54-FILIAL-SACADO-ST        PIC X(005).
               05 SLIG54-CONTROLE-SACADO-ST      PIC 9(002).
               05 FILLER                         PIC X(036).
