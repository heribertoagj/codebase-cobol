      *==============================================================*
      *   BOOK -  I#BVVEHA                                           *
      *           ARQUIVO DE HISTORICO DE ANTECIPACAO PROGRAMADA     *
      ****************************************************************
      *   TAMANHO        0200  BYTES                                 *
      *   TAMANHO CHAVE  0042  BYTES (POSICAO 01 A 40)               *
      ****************************************************************
       01  HT-ANTECIPA-PGM.
           03  HT-CHAVE.
               05  HT-AGENCIA                     PIC  9(0005).
               05  HT-CONTA                       PIC  9(0007).
               05  HT-DIGITO-CONTA                PIC  9(0001).
               05  HT-PRODUTO                     PIC  9(0002).
               05  HT-DT-ANTECIPACAO              PIC  9(0008).
               05  HT-PROTOCOLO                   PIC  9(0009).
               05  HT-SEQUENCIAL                  PIC  9(0010).
           03  HT-MIDIA                           PIC  X(0003).
           03  HT-DT-VCTO-TITLO                   PIC  9(0008).
           03  HT-VLR-BRUTO-TITLO                 PIC  9(0013)V9(02).
           03  HT-VLR-LIQUIDO-TITLO               PIC  9(0013)V9(02).
           03  HT-TAXA-APLICADA                   PIC  9(0003)V9(02).
           03  HT-VLR-IOF                         PIC  9(0013)V9(02).
           03  HT-VLR-TAC                         PIC  9(0005)V9(02).
           03  HT-CODIGO-FUNCIONAL                PIC  9(0009).
           03  HT-NOME-AUTORIZANTE                PIC  X(0040).
           03  FILLER                             PIC  X(0041).
      *    DATAS NO FORMATO AAAAMMDD
