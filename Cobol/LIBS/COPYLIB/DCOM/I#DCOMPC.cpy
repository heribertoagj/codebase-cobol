      *****************************************************************
      * SISTEMA...: DCOM - DESCONTO COMERCIAL        NOME: I#DCOMPC   *
      * DESCRICAO.: ARQUIVO DE PARCELAS PROJETADAS                    *
      * TAMANHO...: LRECL = 100                                       *
      *****************************************************************
       01  PC-REGISTRO.
01 02      03  PC-CTPO-DESC-COML           PIC  9(03)         COMP-3.
03 01      03  PC-TPO-VALOR                PIC  9(01)         COMP-3.
04 05      03  PC-DEVNTO                   PIC  9(08)         COMP-3.
09 09      03  PC-VPRINC-ENTRADA           PIC S9(15)V9(02)   COMP-3.
18 09      03  PC-VPRINC-PCELA-DESC        PIC S9(15)V9(02)   COMP-3.
27 09      03  PC-VPRINC-BAIXA             PIC S9(15)V9(02)   COMP-3.
36 09      03  PC-VRAP-PSENT-PCELA         PIC S9(15)V9(02)   COMP-3.
45 56      03  PC-FILLER                   PIC  X(56).
