      *****************************************************************
      * SISTEMA : CCLZ - CCLZWB05                                     *
      * TAMANHO : 0700 BYTES                                          *
      * ARQUIVO : PROCESSO DE RECEPCAO DE MOVIMENTO                   *
      * HEADER DO MOVIMENTO (TIPO 05)                                 *
      *****************************************************************
       05  CCLZWB05-REG.
        10 CCLZWB05-TPO-REG                   PIC  9(002).
        10 CCLZWB05-CNPJ-DECLT                PIC  9(014).
        10 CCLZWB05-CID-GLOBL-DECLT           PIC  X(019).
        10 CCLZWB05-QTD-REGISTROS             PIC  9(018).
        10 CCLZWB05-CTPO-DECLR-RECTA          PIC  X(001).
        10 CCLZWB05-NSEQ-RQUIS-RETIF          PIC  X(050).
        10 CCLZWB05-VRSAO-LYOUT               PIC  X(004).
        10 CCLZWB05-CCTRO-CUSTO               PIC  X(004).
        10 CCLZWB05-DANO-NMES-REF             PIC  9(006).
        10 CCLZWB05-DHORA-GERAC-ARQ           PIC  X(026).
        10 FILLER                             PIC  X(556).
