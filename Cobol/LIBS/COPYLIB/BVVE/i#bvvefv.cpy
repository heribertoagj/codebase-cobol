      *================================================================*
      *  ARQUIVO COM DADOS DE CONSULTA DA AGENDA                       *
      *                                                                *
      *  I#BVVEFV - 550 BYTES                                          *
      *----------------------------------------------------------------*
      *  -- HEADER                                                     *
      *  CAG-BCRIA         : AGENCIA                                   *
      *  CCTA-BCRIA-CLI    : CONTA                                     *
      *  CCTA-BCRIA-DIG    : DIGITO CONTA                              *
      *  NOME-CLI          : NOME DO CLIENTE                           *
      *  -- DETALHE                                                    *
      *  CPRODT-AGNDA-FINCR: CODIGO DO PRODUTO                         *
      *  IPRODT-AGNDA-FINCR: DESCRICAO DO PRODUTO                      *
      *  DT-LANCTO         : DATA DE LANCAMENTO                        *
      *  VAGNDA-DISPN-OPER : VALOR AGENDA DISPONIVEL                   *
      *================================================================*
      *                                                                *
       01  BVVEFV-REGISTRO.
           05 BVVEFV-TIPO-REGISTRO        PIC 9(001).
      *       2 - DETALHE
           05 BVVEFV-DADOS                PIC X(200).
           05 BVVEFV-HEADER         REDEFINES BVVEFV-DADOS.
              10 BVVEFV-H-CAG-BCRIA       PIC 9(005) COMP-3.
              10 BVVEFV-H-CCTA-BCRIA-CLI  PIC 9(007) COMP-3.
              10 BVVEFV-H-CCTA-BCRIA-DIG  PIC 9(001) COMP-3.
              10 BVVEFV-H-NOME-CLI        PIC X(040).
              10 FILLER                   PIC X(152).
           05 BVVEFV-DETALHE        REDEFINES BVVEFV-DADOS.
              10 BVVEFV-D-CPRODT-AGNDA-FINCR
                                          PIC 9(005) COMP-3.
              10 BVVEFV-D-IPRODT-AGNDA-FINCR
                                          PIC X(020).
              10 BVVEFV-D-DT-LANCTO       PIC X(010).
              10 BVVEFV-D-VAGNDA-DISPN-OPER
                                          PIC S9(013)V99 COMP-3.
              10 FILLER                   PIC X(159).
