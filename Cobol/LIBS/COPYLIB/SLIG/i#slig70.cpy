      ***===========================================================***
      *** NOME INC                                     LENGTH=0322  ***
      *** I#SLIG70                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO REGS.SELECIONADOS TITULOS              ***
      ***                                                           ***
      ***===========================================================***
       01  REG-SELECIONADO.
           03  SLIG70-CTRL.
               05 SLIG70-CGC-CPF-CTRL-ST.
                   07 SLIG70-CGC-CPF-CTRL        PIC 9(009).
               05 SLIG70-PRODUTO-CTRL            PIC X(001).
               05 SLIG70-CARTEIRA-CTRL           PIC 9(001).
               05 SLIG70-CD-PRODUTO-CTRL         PIC 9(002).
               05 SLIG70-CD-CNEGOC-CTRL          PIC 9(011).
               05 SLIG70-CD-NOSNRO-CTRL          PIC 9(011).
               05 SLIG70-DT-ENTRADA-SISTEMA-CTRL PIC 9(008).
               05 SLIG70-DT-VENCTO-PROD-CTRL     PIC 9(008).
           03  SLIG70-IDENT-DOCTO.
               05  SLIG70-CD-PRODUTO-CLI         PIC 9(002).
               05  FILLER                        PIC X(002).
               05  SLIG70-AGENCIA-CLI            PIC 9(004).
               05  FILLER                        PIC X(002).
               05  SLIG70-CONTA-CLI              PIC 9(007).
               05  FILLER                        PIC X(002).
               05  SLIG70-NUM-TIT                PIC 9(011).
               05  FILLER                        PIC X(005).
           03  SLIG70-PERIODO.
               05  SLIG70-DT-PER-INI             PIC 9(008).
               05  SLIG70-DT-PER-FIM             PIC 9(008).
           03  SLIG70-VINCULACAO.
               05 SLIG70-DIR-REG                 PIC 9(005) COMP-3.
               05 SLIG70-GER-REG                 PIC 9(005) COMP-3.
               05 SLIG70-GRP-ECONM               PIC 9(009) COMP-3.
               05 SLIG70-AGENCIA                 PIC 9(005) COMP-3.
           03  SLIG70-REG-TITULO.
               05 SLIG70-BCO-CLIENTE             PIC 9(003) COMP-3.
               05 SLIG70-AGENCIA-CLIENTE         PIC 9(005) COMP-3.
               05 SLIG70-CONTA-CLIENTE           PIC 9(013) COMP-3.
               05 SLIG70-CTPO-CART               PIC 9(003) COMP-3.
               05 SLIG70-CCLI-COBR               PIC 9(009) COMP-3.
               05 SLIG70-CGC-CPF-CLIENTE         PIC 9(009) COMP-3.
               05 SLIG70-FILIAL-CLIENTE          PIC 9(005) COMP-3.
               05 SLIG70-CONTROLE-CLIENTE        PIC 9(002).
               05 SLIG70-BCO-SACADO              PIC 9(003) COMP-3.
               05 SLIG70-AGENCIA-SACADO          PIC 9(005) COMP-3.
               05 SLIG70-CONTA-SACADO            PIC 9(013) COMP-3.
               05 SLIG70-CGC-CPF-SACADO          PIC 9(009) COMP-3.
               05 SLIG70-FILIAL-SACADO           PIC 9(005) COMP-3.
               05 SLIG70-CONTROLE-SACADO         PIC 9(002).
               05 SLIG70-VL-PRINC-PROD           PIC 9(017) COMP-3.
               05 SLIG70-VL-PAGO-PROD            PIC 9(015) COMP-3.
               05 SLIG70-VL-DESC-ABAT-PROD       PIC 9(015) COMP-3.
               05 SLIG70-DT-VENCTO-PROD          PIC 9(009) COMP-3.
               05 SLIG70-DT-PAGTO-PROD           PIC 9(009) COMP-3.
               05 SLIG70-DT-ENTRADA-ORIGEM       PIC 9(009) COMP-3.
               05 SLIG70-DT-ENTRADA-SISTEMA      PIC 9(009) COMP-3.
               05 SLIG70-DT-VENCTO-ANTERIOR      PIC 9(009) COMP-3.
               05 SLIG70-CD-PRODUTO              PIC 9(002).
               05 SLIG70-CD-CNEGOC               PIC 9(011) COMP-3.
               05 SLIG70-CD-NOSNRO               PIC 9(011) COMP-3.
               05 SLIG70-CD-SEQUENCIA            PIC 9(003) COMP-3.
               05 SLIG70-CD-COD-NTAX             PIC 9(005) COMP-3.
               05 SLIG70-CD-OCO-REM-L            PIC 9(003) COMP-3.
               05 SLIG70-CD-MOT-REM-SIS          PIC 9(003) COMP-3.
               05 SLIG70-CD-ESPECIE              PIC 9(003) COMP-3.
               05 SLIG70-CD-ORIGEM               PIC 9(003) COMP-3.
               05 SLIG70-DT-INICIO-PER           PIC 9(009) COMP-3.
               05 SLIG70-DT-FINAL-PER            PIC 9(009) COMP-3.

      * === CAMPOS DUPLICADOS PARA STRING ===
               05 SLIG70-CGC-CPF-CLIENTE-ST      PIC X(009).
               05 SLIG70-FILIAL-CLIENTE-ST       PIC X(005).
               05 SLIG70-CONTROLE-CLIENTE-ST     PIC 9(002).
               05 SLIG70-CGC-CPF-SACADO-ST       PIC X(009).
               05 SLIG70-FILIAL-SACADO-ST        PIC X(005).
               05 SLIG70-CONTROLE-SACADO-ST      PIC 9(002).
               05 FILLER                         PIC X(036).
