      *****************************************************************
      * NOME DA INC - I#GFCT66                                        *
      * DESCRICAO   - GERACAO MOVTO P/ COBRANCA DE FICHA CADASTRAL    *
      *               COPIA DA I#GFCT61 - VERSAO PARA SBAT            *
      *                                   SEM CAMPOS COMPACTADOS      *
      * TAMANHO     - 0045                                            *
      * DATA        - 06.2008                                         *
      * RESPONSAVEL - WAGNER - PROCWORK                               *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  REG-GFCT66.
           03  GFCT66-AGENCIA                       PIC  9(05).
           03  GFCT66-CONTA                         PIC  9(13).
ST2507*    03  GFCT66-CPF-NUM                       PIC  9(09).
ST2507     03  GFCT66-CPF-NUM                       PIC  X(09).
           03  GFCT66-CPF-CTR                       PIC  9(02).
           03  GFCT66-DT-ABERTURA-CTA               PIC  9(08).
           03  GFCT66-TIPO-TRATO                    PIC  X(01).
      *        1 - ABERTURA ANTERIOR A DATA SOLICITADA
      *        2 - ABERTURA POSTERIOR A DATA SOLICITADA
           03  FILLER                               PIC  X(07).
