      ******************************************************************
      * SISTEMA : DCOM - ESTATISTICAS NET EMPRESA -          I#DCOMU2  *
      * TAMANHO : 0100 BYTES   BAIXAS COMPLEMENTADO                    *
      * ARQUIVO : BAIXA COMPLEMENTADA COM PRODUTO E SUBPRODUTO         *
      * INCLUDE DA TABELA DCOMB094 - DB2PRD.TBAIXA_PCELA_DESC          *
      ******************************************************************
001    01 U2-REGISTRO.
001       05 U2-CHAVE.
001          10 U2-CCORR.
001 02          15 U2-CBCO             PIC  S9(003)V USAGE COMP-3.
003 03          15 U2-CAG-BCRIA        PIC  S9(005)V USAGE COMP-3.
006 07          15 U2-CCTA-CRIA-CLI    PIC  S9(013)V USAGE COMP-3.
013 02       10 U2-CPRODT              PIC S9(3)V USAGE COMP-3.
015 02       10 U2-CSPROD-DESC-COML    PIC S9(3)V USAGE COMP-3.
017 08       10 U2-DEFETV-BAIXA-PCELA  PIC 9(08).
017 08       10 FILLER REDEFINES       U2-DEFETV-BAIXA-PCELA.
017 04          15 U2-DANO-BAIXA       PIC 9(4).
021 02          15 U2-DMES-BAIXA       PIC 9(2).
023 02          15 U2-DDIA-BAIXA       PIC 9(2).
025 09    05 U2-VPRINC-BAIXA-PCELA     PIC S9(15)V9(2) USAGE COMP-3.
034 08    05 U2-DVCTO-OPER-DESC        PIC  9(08).
034       05 FILLER REDEFINES          U2-DVCTO-OPER-DESC.
034 04       10 U2-DANO-VCTO           PIC 9(4).
038 02       10 U2-DMES-VCTO           PIC 9(2).
040 02       10 U2-DDIA-VCTO           PIC 9(2).
042 02    05 U2-CMEIO-PGTO-DESC        PIC S9(3)V USAGE COMP-3.
044 02    05 U2-CBCO-PGTO              PIC S9(3)V USAGE COMP-3.
044 02    05 U2-INDIC-BAIXA-PCELA      PIC X(1).
046 54    05 FILLER                    PIC X(054).
