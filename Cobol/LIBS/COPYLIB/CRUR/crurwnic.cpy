      ******************************************************************
      * NOME BOOK : CRURWNIC                                           *
      * DESCRICAO : CONSULTAR PARAMETRO NIRF                           *
      * DATA      : 06/12/2024                                         *
      * AUTOR     : MILTON JANUARIO                                    *
      * EMPRESA   : BRADESCO                                           *
      * COMPONENTE: CRUR                                               *
      * TAMANHO   : 465 BYTES                                          *
      ******************************************************************
      * AREA DE ENTRADA:                                               *
      * ----------------                                               *
      *    NSEQ-CONTR-NEGOC.....: NUMERO DO CONTRATO                   *
      *                                                                *
      ******************************************************************
      * AREA DE SAIDA:                                                 *
      * ----------------                                               *
      *    QTD-OCORR............: QUANTIDADE DE OCORRENCIAS            *
      *    CIMOV-CREDT-RURAL....: CODIGO DO IMOVEL                     *
      *    CMATR-IMOV-RURAL.....: MATRICULA DO IMOVEL                  *
      *    CNIRF-IMOV-RURAL.....: NUMERO DO NIRF/CCIR                  *
      ******************************************************************
           05 CRURWNIC-HEADER.
             10 CRURWNIC-COD-LAYOUT    PIC  X(08)      VALUE 'CRURWNIC'.
             10 CRURWNIC-TAM-LAYOUT    PIC  9(05)      VALUE 785.
           05 CRURWNIC-REGISTRO.
             10 CRURWNIC-BLOCO-ENTRADA.
               15 CRURWNIC-E-NSEQ-CONTR-NEGOC      PIC  9(009).
             10 CRURWNIC-BLOCO-SAIDA.
               15 CRURWNIC-S-QTD-OCORR             PIC  9(003).
               15 CRURWNIC-S-OCORRENCIAS OCCURS 20 TIMES.
                 20 CRURWNIC-S-CIMOV-CREDT-RURAL   PIC  9(010).
                 20 CRURWNIC-S-CMATR-IMOV-RURAL    PIC  X(020).
                 20 CRURWNIC-S-CNIRF-IMOV-RURAL    PIC  X(008).
