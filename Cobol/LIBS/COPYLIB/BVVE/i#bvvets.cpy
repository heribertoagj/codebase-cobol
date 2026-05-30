      *================================================================*
      *  I#BVVETS - AREA DE COMUNICACAO COM O MODULO BVVE0372          *
      *             CONSULTA DE SALDO E MEDIA HISTORICA                *
      *----------------------------------------------------------------*
      *  LRECL: 149                                                    *
      *----------------------------------------------------------------*
      *  BVVETS-E-BANDEIRA - BCPP VISAO 2                              *
      *================================================================*
       01  BVVETS-COMMAREA.
           05 BVVETS-HEADER.
              10 BVVETS-COD-LAYOUT         PIC  X(008) VALUE 'I#BVVETS'.
              10 BVVETS-TAM-LAYOUT         PIC  9(005) VALUE 149.
      *
           05 BVVETS-BLOCO-REGISTRO.
              10 BVVETS-BLOCO-ENTRADA.
                 15 BVVETS-E-NUM-CPF-CNPJ  PIC  9(010).
                 15 BVVETS-E-FILIAL        PIC  9(004).
                 15 BVVETS-E-CONTROLE      PIC  9(002).
                 15 BVVETS-E-AGENCIA       PIC  9(005).
                 15 BVVETS-E-CONTA-AUX     PIC  9(008).
                 15 BVVETS-E-CONTA-R   REDEFINES BVVETS-E-CONTA-AUX.
                    20 FILLER              PIC  9(001).
                    20 BVVETS-E-CONTA      PIC  9(007).
                 15 BVVETS-E-BANDEIRA      PIC  9(003).
      *
              10 BVVETS-BLOCO-SAIDA.
                 15 BVVETS-S-VLR-SALDO     PIC  9(015)V99.
                 15 BVVETS-S-VLR-MEDIA     PIC  9(013)V99.
                 15 BVVETS-S-COD-RETORNO   PIC  9(002).
                 15 BVVETS-S-MSG-RETORNO   PIC  X(070).
