      *I#DCOMNC
      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNC *
      * CRIACAO   : 02/2007                                   *
      * DESCRICAO : INTERFACE PFEB X DCOM - BOLETOS BRADESCO  *
      *             INFORMACAO DOS AGENDAMENTOS EFETIVAMENTE  *
      *             DESCONTADOS, QUE SE REFERIAM A BOLETOS DO *
      *             BRADESCO.                                 *
      *             (PARA O DCOM SOLICITAR BAIXA DO MESMO PARA*
      *              O SISTEMA DE COBRANCA).                  *
      * TAMANHO   : LRECL  064                                *
      *********************************************************
       01      DCOMNC-BAIXAR-BOLETOS-BRADESCO.
           05    DCOMNC-CHV-PFEB.
             10  DCOMNC-CIDTFD-SEQ-PGTO  PIC  X(16).
             10  DCOMNC-CORIGE-PGTO      PIC S9(01)V COMP-3.
             10  DCOMNC-CCGC-CPF         PIC S9(09)V COMP-3.
             10  DCOMNC-CFLIAL-CGC       PIC S9(05)V COMP-3.
             10  DCOMNC-CCCTRL-CGC       PIC S9(02)V COMP-3.
           05    DCOMNC-DTA-AGEND        PIC  9(09)  COMP-3.
           05    DCOMNC-VLR-AGEND      PIC S9(15)V99 COMP-3.
           05    DCOMNC-NRO-OP-DESCONTO  PIC  9(11)  COMP-3.
           05    DCOMNC-BANCO            PIC  9(03)  COMP-3.
           05    DCOMNC-CARTEIRA         PIC  9(02).
           05    DCOMNC-AGENCIA          PIC  9(05)  COMP-3.
           05    DCOMNC-CONTA            PIC  9(13)  COMP-3.
           05    DCOMNC-NNUMERO-COBR     PIC  9(18)V COMP-3.
           05    DCOMNC-NRO-SEQ-TITULO   PIC  9(03)V COMP-3.
           05    FILLER                  PIC  X(07).
      *
