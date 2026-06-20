      ******************************************************************
      * NOME DA INC - I#GFCTMW                                         *
      * DESCRICAO   - REGISTROS QUE PASSARAM PELO PROCESSO DE COBRANCA *
      * TAMANHO     - 0340                                             *
      * DATA        - 09.2007                                          *
      * RESPONSAVEL - WAGNER - PROCWORK                                *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 REG-GFCTMW.
          05 GFCTMW-CHAVE-COBRANCA.
             10 GFCTMW-DATA-ENVIO         PIC 9(08).
             10 GFCTMW-DIG-CONTRATO       PIC X(01).
             10 GFCTMW-NUM-CONTRATO       PIC 9(07).
          05 GFCTMW-CHAVE-ORIGEM.
             10 GFCTMW-DATA-ORIG          PIC X(10).
             10 GFCTMW-ROTINA-ORIG        PIC X(04).
             10 GFCTMW-NUM-ARQ-ORIG       PIC 9(02).
             10 GFCTMW-SEQ-ARQ-ORIG       PIC 9(11).
             10 GFCTMW-TARIFA-ORIG        PIC 9(05).
          05 GFCTMW-CGC-CPF.
ST2507*      10 GFCTMW-CGC-CPF-MUNERO     PIC 9(09).
ST2507*      10 GFCTMW-CGC-CPF-FILIAL     PIC 9(05).
ST2507       10 GFCTMW-CGC-CPF-MUNERO     PIC X(09).
ST2507       10 GFCTMW-CGC-CPF-FILIAL     PIC X(04).
             10 GFCTMW-CGC-CPF-CONTROLE   PIC 9(02).
          05 GFCTMW-DADOS-DEBITO.
             10 GFCTMW-BANCO-DEB          PIC 9(03).
             10 GFCTMW-AGENCIA-DEB        PIC 9(05).
             10 GFCTMW-CONTA-DEB          PIC 9(07).
             10 GFCTMW-DIGITO-DEB         PIC X(01).
             10 GFCTMW-CARTEIRA-DEB       PIC 9(05).
             10 GFCTMW-COD-LCTO-DEB       PIC 9(05).
             10 GFCTMW-COD-MOEDA-DEB      PIC 9(05).
             10 GFCTMW-RAZAO.
                15 GFCTMW-GRUPO-DEB       PIC 9(03).
                15 GFCTMW-SUBGRUPO-DEB    PIC 9(03).
             10 GFCTMW-VALOR-DEBITAR      PIC 9(11)V9(2).
          05 GFCTMW-DETALHES-COBRANCA.
             10 GFCTMW-SITUACAO-EVENTO    PIC 9(02).
             10 GFCTMW-TIPO-COBRANCA      PIC 9(02).
             10 GFCTMW-TIPO-ARQUIVO       PIC 9(02).
             10 GFCTMW-DEB-PARCIAL        PIC 9(02).
             10 GFCTMW-TIPO-SALDO         PIC 9(02).
             10 GFCTMW-TIPO-CONTA         PIC 9(01).
             10 GFCTMW-INCONDICIONAL      PIC X(01).
             10 GFCTMW-ACEITA-PARCELADO   PIC X(01).
             10 GFCTMW-PARCIAL-ULT-REPIQ  PIC X(01).
             10 GFCTMW-VALOR-MINIMO       PIC 9(11)V9(2).
             10 GFCTMW-BANCO-ORIG         PIC 9(03).
             10 GFCTMW-AGENCIA-ORIG       PIC 9(05).
             10 GFCTMW-DATA-EVNTO-ORIG    PIC 9(08).
             10 GFCTMW-SEG-LINHA-EXTRATO  PIC X(32).
          05 GFCTMW-RETORNO.
             10 GFCTMW-JOB                PIC X(08).
             10 GFCTMW-PROGRAMA           PIC X(08).
             10 GFCTMW-RECURSO            PIC X(08).
             10 GFCTMW-COD-RETORNO        PIC X(03).
             10 GFCTMW-VALOR-DEBITADO     PIC 9(11)V9(2).
             10 GFCTMW-DESCRICAO          PIC X(50).
          05 GFCTMW-TRAG-EXT.
             10 GFCTMW-CENTRALIZ          PIC X(01).
             10 GFCTMW-AGENCIA-RET        PIC 9(05).
             10 GFCTMW-CONTA-RET          PIC 9(07).
          05 GFCTMW-VALORES-TAB92.
             10 GFCTMW-VALOR-LIQ92        PIC 9(11)V9(2).
             10 GFCTMW-VALOR-COBR92       PIC 9(11)V9(2).
             10 GFCTMW-TENTV-DEB-ATU      PIC 9(03).
             10 GFCTMW-TENTV-DEB-MAX      PIC 9(03).
ST2507*   05 FILLER                       PIC X(16).
ST2507    05 FILLER                       PIC X(17).
      *
      *****************************************************************
