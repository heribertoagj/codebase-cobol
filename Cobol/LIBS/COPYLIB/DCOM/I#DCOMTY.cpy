      ******************************************************************
      * SISTEMA : DCOM - I#DCOMTY                                      *
      * TAMANHO : 0256 BYTES                                           *
      * ARQUIVO : TARIFA      (PRODUCAO DIARIA/MENSAL)                 *
      ******************************************************************
       01  TY-REGISTRO.
      *    --->> CHAVE
  1  5     03  TY-CJUNC-DEPDC               PIC  9(05).
  6 13     03  TY-CCCORR                    PIC  9(13).
 19  9     03  TY-CCNPJ-CPF                 PIC  9(09).
 28  5     03  TY-CFLIAL-CNPJ               PIC  9(05).
 33  2     03  TY-CCTRL-CNPJ                PIC  9(02).
 35  5     03  TY-CPOSTO-SERVC              PIC  9(05).
 40  2     03  TY-CTPO-PAB                  PIC  X(02).
 42  3     03  TY-SEGMENTO-CLIE             PIC  9(03).
 45  3     03  TY-CPRODT                    PIC  9(03).
 48  3     03  TY-CSPROD-DESC               PIC  9(03).
 51  2     03  TY-CINDCD-CONTR-CONVE        PIC  X(02).
 53  9     03  TY-CCONTR-CONVE-DESC         PIC  9(09).
 62  3     03  TY-CVRSAO-CONTR-CONVE        PIC  9(03).
      *    --->> ATRIBUTOS
 65  4     03  TY-DANO-OPER-DESC            PIC  9(04).
 69  9     03  TY-NSEQ-OPER-DESC            PIC  9(09).
 78  3     03  TY-CTPO-GARANTIA-PRINC       PIC  9(03).
 81  3     03  TY-CTPO-GARANTIA-COMPL       PIC  9(03).
 84 17     03  TY-VTAC-OPER-DESC            PIC  9(15)V9(02).
101  3     03  TY-COD-LANCAMENTO            PIC  9(03).
104  5     03  TY-CGRP-CTBIL-CRED           PIC  9(05).
109 17     03  TY-VOPER-DESC-COML           PIC  9(15)V9(02).
126 17     03  TY-VTARIF-TITLO-OPER-DESC    PIC  9(15)V9(02).
143114     03  FILLER                       PIC X(114).
