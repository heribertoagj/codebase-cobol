      **************************************************************
      *  ARQUIVO  MOVIMENTO DE TITULOS DESCONTADOS                 *
      *  TAMANHO DO ARQUIVO =  0070 POSICOES.                      *
      *  NOME  DA  INC      =  I#DCOMWR                            *
      *  FORMATO DAS DATAS  =  (AAAAMMDD)                          *
      *  OBS: COPIA DA INC I#DECEAF                                *
      **************************************************************
       01  REG-TITULO.
           03  TIT-MES-BASE                 PIC  9(02).
           03  TIT-ANO-BASE                 PIC  9(04).
           03  TIT-CHAVE.
               05  TIT-AGENCIA              PIC  9(05)       COMP-3.
               05  TIT-CONTA                PIC  9(13)       COMP-3.
               05  TIT-CARTEIRA             PIC  X(03).
               05  TIT-CONTRATO             PIC  9(17)       COMP-3.
           03  TIT-DT-VENCIMENTO            PIC  9(09)       COMP-3.
           03  TIT-VL-TITULO                PIC S9(15)V9(02) COMP-3.
           03  TIT-DT-PAGAMENTO             PIC  9(09)       COMP-3.
           03  TIT-VL-PAGAMENTO             PIC S9(15)V9(02) COMP-3.
           03  TIT-MOTIVO                   PIC  X(01).
           03  TIT-DT-MOVTO                 PIC  9(09)       COMP-3.
           03  FILLER                       PIC  X(08).
