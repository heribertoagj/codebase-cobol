      *================================================================*
      *  I#BVVET5 - AREA DE COMUNICACAO COM MODULO BVVE0376            *
      *             ENVIO DE TRAVA PARA CIP NO BLOQUEIO DE GARANTIA    *
      *----------------------------------------------------------------*
      *  LRECL: 120                                                    *
      *----------------------------------------------------------------*
      *  BVVET5-E-PRAZO     - AAAAMMDD                                 *
      *  BVVET5-S-SIT-TRAVA - 01 JA TRAVADO                            *
      *                       02 ALTERACAO PRAZO                       *
      *                       03 INCLUSAO DE TRAVA RAIZ                *
      *                       04 INCLUSAO DE TRAVA CPF                 *
      *                       05 INCLUSAO DE TRAVA POR FILIAL          *
      *================================================================*
       01  BVVET5-COMMAREA.
           05 BVVET5-HEADER.
              10 BVVET5-COD-LAYOUT         PIC  X(008) VALUE 'I#BVVET5'.
              10 BVVET5-TAM-LAYOUT         PIC  9(005) VALUE 120.
      *
           05 BVVET5-BLOCO-REGISTRO.
              10 BVVET5-BLOCO-ENTRADA.
                 15 BVVET5-E-CNPJ-CPF      PIC  9(009).
                 15 BVVET5-E-TP-PSSOA      PIC  X(001).
                 15 BVVET5-E-BAND-CIP.
                    20 BVVET5-E-BAND-CIP-N PIC  9(003).
                 15 BVVET5-E-AGENCIA       PIC  9(005).
                 15 BVVET5-E-CONTA-AUX     PIC  9(008).
                 15 BVVET5-E-CONTA-R   REDEFINES BVVET5-E-CONTA-AUX.
                    20 FILLER              PIC  9(001).
                    20 BVVET5-E-CONTA      PIC  9(007).
                 15 BVVET5-E-PRAZO         PIC  9(008).
                 15 BVVET5-E-VALOR         PIC  9(015)V99.
      *
              10 BVVET5-BLOCO-SAIDA.
                 15 BVVET5-S-COD-RETORNO   PIC  9(002).
                 15 BVVET5-S-SIT-TRAVA     PIC  9(002).
                 15 BVVET5-S-MSG-RETORNO   PIC  X(052).
      *================================================================*
