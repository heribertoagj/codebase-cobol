      ***===========================================================***
      *** NOME INC                                     LENGTH=0308  ***
      *** I#SLIG9W                                                  ***
      ***-----------------------------------------------------------***
      **                                                             **
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVO IMAGEM DA TABELA  TARQ_INDIC_PRODT      **
      **             PARA CONTROLE DE RECEPCAO DE ARQUIVOS.          **
      ***===========================================================***
       01 REG-TARQ-INDIC-PRODT.
          03 SLIG9W-NARQ-INDIC-PRODT     PIC S9(9)       COMP-3.
          03 SLIG9W-CIDTFD-PRODT-LIQDZ   PIC S9(2)       COMP-3.
          03 SLIG9W-DMOVTO-INDIC-PRODT   PIC X(10).
          03 SLIG9W-IARQ-INDIC-PRODT     PIC X(80).
          03 SLIG9W-QARQ-INDIC-PRODT     PIC S9(15)      COMP-3.
          03 SLIG9W-VARQ-INDIC-PRODT     PIC S9(16)V9(2) COMP-3.
          03 SLIG9W-CARQ-INDIC-PRODT     PIC S9(1)V      COMP-3.
          03 SLIG9W-HATULZ               PIC X(26).
