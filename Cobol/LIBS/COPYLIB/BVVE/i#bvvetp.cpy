      *================================================================*
      *  I#BVVETP - AREA DE COMUNICACAO COM MODULO BVVE0374            *
      *             SOLICITACAO DE DESBLOQUEIO DE SALDO EM GARANTIA    *
      *----------------------------------------------------------------*
      *  LRECL: 151                                                    *
      *----------------------------------------------------------------*
      *  BVVETP-E-BANDEIRA - BCPP VISAO 2                              *
      *================================================================*
       01  BVVETP-COMMAREA.
           05 BVVETP-HEADER.
              10 BVVETP-COD-LAYOUT         PIC  X(008) VALUE 'I#BVVETP'.
              10 BVVETP-TAM-LAYOUT         PIC  9(005) VALUE 151.
      *
           05 BVVETP-BLOCO-REGISTRO.
              10 BVVETP-BLOCO-ENTRADA.
                 15 BVVETP-E-NUM-CPF-CNPJ  PIC  9(010).
                 15 BVVETP-E-FILIAL        PIC  9(004).
                 15 BVVETP-E-CONTROLE      PIC  9(002).
                 15 BVVETP-E-AGENCIA       PIC  9(005).
                 15 BVVETP-E-CONTA-AUX     PIC  9(008).
                 15 BVVETP-E-CONTA-R   REDEFINES BVVETP-E-CONTA-AUX.
                    20 FILLER              PIC  9(001).
                    20 BVVETP-E-CONTA      PIC  9(007).
                 15 BVVETP-E-BANDEIRA.
                    20 BVVETP-E-BANDEIRA-N PIC  9(003).
                 15 BVVETP-E-VLR-DESBLOQ   PIC  9(015)V99.
      *
              10 BVVETP-BLOCO-SAIDA.
                 15 BVVETP-S-VLR-DESBLOQ   PIC  9(015)V99.
                 15 BVVETP-S-COD-RETORNO   PIC  9(02).
                 15 BVVETP-S-MSG-RETORNO   PIC  X(70).
