      *==============================================================*
      *   BOOK -  I#BVVEAP                                           *
      *           ARQUIVO DE CADATRO DE ANTECIPACAO PROGRAMADA       *
      ****************************************************************
      *   TAMANHO        0150  BYTES                                 *
      *   TAMANHO CHAVE  0015  BYTES (POSICAO 01 A 15)               *
      ****************************************************************
       01  CD-ANTECIPA-PGM.
           03  CD-CHAVE.
               05  CD-AGENCIA                     PIC  9(0005).
               05  CD-CONTA                       PIC  9(0007).
               05  CD-DIGITO-CONTA                PIC  9(0001).
               05  CD-PRODUTO                     PIC  9(0002).
           03  CD-OPCAO-PROGRAMADO.
               05  CD-OPCAO-PGM-SEGUNDA           PIC  X(0001).
               05  CD-OPCAO-PGM-TERCA             PIC  X(0001).
               05  CD-OPCAO-PGM-QUARTA            PIC  X(0001).
               05  CD-OPCAO-PGM-QUINTA            PIC  X(0001).
               05  CD-OPCAO-PGM-SEXTA             PIC  X(0001).
               05  CD-OPCAO-PGM-DIARIO            PIC  X(0001).
           03  CD-DESATIVACAO.
               05  CD-DT-INICIAL-DESATIVADO       PIC  9(0008).
               05  CD-DT-FINAL-DESATIVADO         PIC  9(0008).
           03  CD-TAXA.
               05  CD-TIPO-TAXA                   PIC  X(0001).
               05  CD-PERC-TAXA                   PIC  9(0003)V9(03).
           03  CD-AUTORIZANTE.
               05  CD-COD-AUTORIZANTE             PIC  X(0009).
               05  CD-NOME-AUTORIZANTE            PIC  X(0040).
           03  CD-DT-INCLUSAO                     PIC  9(0008).
           03  CD-DT-ALTERACAO                    PIC  9(0008).
           03  FILLER                             PIC  X(0041).

      *-----------> OPCAO DO PROGRAMADO
      *-------->  'X' PARA OS DIAS DA SEMANA PROGRAMADO OU
      *-------->  'X' PARA PARA A OPCAO DIARIA.

      *-----------> DATAS DE DESATIVACAO - FORMATO (AAAAMMDD)
      *-------->  DATAS PREENCHIDAS SE HOUVER PERIODO DE DESATIVACAO.

      *-----------> TIPO DE TAXA
      *-------->  N (NORMAL)
      *-------->  E (ESPECIAL)

      *-----------> TAXA
      *-------->  VALOR DA TAXA APENAS SE FOR "E" (ESPECIAL)

      *-----------> DATAS DE INCLUSAO E ALTERACAO - FORMATO (AAAAMMDD)
