      ******************************************************************
      * NOME BOOK : DCOMWAHW - INTERFACE DE PERSISTENCIA DO COORDENADOR*
      * DESCRICAO : BUSCAR DADOS DO DOCUMENTO DDC                      *
      * DATA      : 21/03/2023                                         *
      * EMPRESA   : CAPGEMINI                                          *
      * ANALISTA  : LEVI BARCIELA DA SILVA CHAVES                      *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 122 BYTES                                          *
      ******************************************************************
      *  DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
           05  DCOMWAHW-HEADER.
               10  DCOMWAHW-COD-LAYOUT       PIC X(08) VALUE 'DCOMWAHW'.
               10  DCOMWAHW-TAM-LAYOUT       PIC 9(05) VALUE 00122.
           05  DCOMWAHW-REGISTRO.
               10  DCOMWAHW-COD-RETORNO               PIC  9(02).
               10  DCOMWAHW-BLOCO-ENTRADA.
                   15  DCOMWAHW-DANO-OPER-DESC    PIC  9(04).
                   15  DCOMWAHW-NSEQ-OPER-DESC    PIC  9(09).
                   15  DCOMWAHW-NPCELA            PIC  9(05).
                   15  DCOMWAHW-CBCO              PIC  9(03).
                   15  DCOMWAHW-CAG-BCRIA         PIC  9(05).
                   15  DCOMWAHW-CCTA-CORR-BCRIA   PIC  9(11).
                   15  DCOMWAHW-CCHEQ             PIC  9(07).
               10  DCOMWAHW-BLOCO-PAGINACAO.
                   15  DCOMWAHW-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMWAHW-P-INICIAL          VALUE 'I'.
                       88  DCOMWAHW-P-PRIMEIRA         VALUE 'P'.
                       88  DCOMWAHW-P-SEGUINTE         VALUE 'S'.
                       88  DCOMWAHW-P-ANTERIOR         VALUE 'A'.
                       88  DCOMWAHW-P-ULTIMA           VALUE 'U'.
                   15  DCOMWAHW-CHAVE-INICIO.
                       20  DCOMWAHW-I-NPCELA-DESC-COML PIC 9(05).
                       20  DCOMWAHW-I-CBCO             PIC 9(03).
                       20  DCOMWAHW-I-CAG-BCRIA        PIC 9(05).
                       20  DCOMWAHW-I-CCTA-CORR-BCRIA  PIC 9(11).
                       20  DCOMWAHW-I-CCHEQ            PIC 9(07).
                   15  DCOMWAHW-CHAVE-FINAL.
                       20  DCOMWAHW-F-NPCELA-DESC-COML PIC 9(05).
                       20  DCOMWAHW-F-CBCO             PIC 9(03).
                       20  DCOMWAHW-F-CAG-BCRIA        PIC 9(05).
                       20  DCOMWAHW-F-CCTA-CORR-BCRIA  PIC 9(11).
                       20  DCOMWAHW-F-CCHEQ            PIC 9(07).
