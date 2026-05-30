      *================================================================*
      *  I#BVVETT - AREA DE COMUNICACAO COM O MODULO BVVE0373          *
      *             BLOQUEIO DE SALDO EM GARANTIA E CLIENTE NA CIP     *
      *----------------------------------------------------------------*
      *  LRECL: 160                                                    *
      *----------------------------------------------------------------*
      *  BVVETT-E-BANDEIRA - BCPP VISAO 2                              *
      *  BVVETT-E-DT-OPER  - AAAAMMDD                                  *
      *  BVVETT-E-TP-OPER  - 1 PERFORMADO / 2 A PERFORMAR / 3 AMBOS    *
      *================================================================*
       01  BVVETT-COMMAREA.
           05 BVVETT-HEADER.
              10 BVVETT-COD-LAYOUT         PIC  X(008) VALUE 'I#BVVETT'.
              10 BVVETT-TAM-LAYOUT         PIC  9(005) VALUE 160.
      *
           05 BVVETT-BLOCO-REGISTRO.
              10 BVVETT-BLOCO-ENTRADA.
                 15 BVVETT-E-CPF-CNPJ-AUX  PIC  9(010).
                 15 BVVETT-E-CPF-CNPJ-R REDEFINES BVVETT-E-CPF-CNPJ-AUX.
                    20 FILLER              PIC  9(001).
                    20 BVVETT-E-CPF-CNPJ   PIC  9(009).
                 15 BVVETT-E-FILIAL        PIC  9(004).
                 15 BVVETT-E-CONTROLE      PIC  9(002).
                 15 BVVETT-E-AGENCIA       PIC  9(005).
                 15 BVVETT-E-CONTA-AUX     PIC  9(008).
                 15 BVVETT-E-CONTA-R   REDEFINES BVVETT-E-CONTA-AUX.
                    20 FILLER              PIC  9(001).
                    20 BVVETT-E-CONTA      PIC  9(007).
                 15 BVVETT-E-BANDEIRA      PIC  9(003).
                 15 BVVETT-E-DT-OPER       PIC  9(008).
                 15 BVVETT-E-VLR-BLOQ      PIC  9(015)V99.
                 15 BVVETT-E-TP-OPER       PIC  X(001).
      *
              10 BVVETT-BLOCO-SAIDA.
                 15 BVVETT-S-VLR-BLOQUEADO PIC  9(015)V99.
                 15 BVVETT-S-COD-RETORNO   PIC  9(002).
                 15 BVVETT-S-MSG-RETORNO   PIC  X(070).
