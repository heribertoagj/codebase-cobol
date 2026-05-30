      ******************************************************************
      * NOME BOOK : DCOMWAHI - BOOK DE INTERFACE                       *
      * DESCRICAO : BUSCAR DADOS DO DOCUMENTO DDC                      *
      * DATA      : 21/03/2023                                         *
      * EMPRESA   : CAPGEMINI                                          *
      * ANALISTA  : LEVI BARCIELA DA SILVA CHAVES                      *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 11096 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05  DCOMWAHI-HEADER.
               10  DCOMWAHI-COD-LAYOUT     PIC  X(08)  VALUE 'DCOMWAHI'.
               10  DCOMWAHI-TAM-LAYOUT     PIC  9(05)  VALUE 11028.
           05  DCOMWAHI-REGISTRO.
               10  DCOMWAHI-BLOCO-ENTRADA.
                   15  DCOMWAHI-E-DANO-OPER-DESC    PIC  9(04).
                   15  DCOMWAHI-E-NSEQ-OPER-DESC    PIC  9(09).
                   15  DCOMWAHI-E-NPCELA            PIC  9(05).

               10 DCOMWAHI-BLOCO-PAGINACAO.
                   15  DCOMWAHI-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMWAHI-P-INICIAL           VALUE 'I'.
                       88  DCOMWAHI-P-PRIMEIRA          VALUE 'P'.
                       88  DCOMWAHI-P-SEGUINTE          VALUE 'S'.
                       88  DCOMWAHI-P-ANTERIOR          VALUE 'A'.
                       88  DCOMWAHI-P-ULTIMA            VALUE 'U'.
                   15  DCOMWAHI-CHAVE-INICIO.
                       20  DCOMWAHI-I-NPCELA-DESC-COML  PIC 9(05).
                       20  DCOMWAHI-I-QTD-DESPREZAR     PIC 9(05).
                   15  DCOMWAHI-CHAVE-FINAL.
                       20  DCOMWAHI-F-NPCELA-DESC-COML  PIC 9(05).
                       20  DCOMWAHI-F-QTD-DESPREZAR     PIC 9(05).

               10  DCOMWAHI-BLOCO-SAIDA.
                   15  DCOMWAHI-S-REG-CONTRATO.
                       20  DCOMWAHI-S-IPSSOA-DESC-COML   PIC  X(60).
                       20  DCOMWAHI-S-ISPROD-DESC-COML   PIC  X(40).
                       20  DCOMWAHI-S-NRO-CONTRATO.
                         25  DCOMWAHI-S-DANO-OPER-DESC   PIC  9(04).
                         25  DCOMWAHI-S-NSEQ-OPER-DESC   PIC  9(09).
                       20  DCOMWAHI-S-SIT-PAGAMENTO      PIC  X(20).
                       20  DCOMWAHI-S-DVCTO-FNAL-OPER    PIC  X(10).
                       20  DCOMWAHI-S-PTX-JURO-MES       PIC 9(03)V9(5).
                       20  DCOMWAHI-S-PTX-JURO-ANO       PIC 9(03)V9(5).
                       20  DCOMWAHI-S-VCUSTO-DESC-MES    PIC 9(08)V9(7).
                       20  DCOMWAHI-S-VCUSTO-DESC-ANO    PIC 9(08)V9(7).
                       20  DCOMWAHI-S-VJURO-OPER-DESC    PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VIOF-OPER-DESC     PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VTARIF-REG-TITLO   PIC 9(15)V9(2).
                       20  DCOMWAHI-S-CPF-CNPJ.
                         25  DCOMWAHI-S-CCNPJ-CPF        PIC  9(09).
                         25  DCOMWAHI-S-CFLIAL-CPF       PIC  9(05).
                         25  DCOMWAHI-S-CCTRL-CNPJ-CPF   PIC  9(02).
                       20  DCOMWAHI-S-VOPER-DESC-COML    PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VLR-TOT-LIQ        PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VLIBRC-DESC-COML   PIC 9(15)V9(2).
                       20  DCOMWAHI-S-PERC-LIB           PIC 9(03)V9(2).
                       20  DCOMWAHI-S-DINIC-OPER-DESC    PIC  X(10).
                       20  DCOMWAHI-S-DLIBRC-DESC-COML   PIC  X(10).
                       20  DCOMWAHI-S-CTPO-DESC-COML     PIC  9(03).
                       20  DCOMWAHI-S-ITPO-DESC-COML     PIC  X(30).
                       20  DCOMWAHI-S-LIBERACAO-CALC     PIC 9(15)V9(2).
                       20  DCOMWAHI-S-SALDO-DEVEDOR      PIC 9(15)V9(2).
                       20  DCOMWAHI-S-TMED-PONDE-OPER    PIC  9(03).
                       20  DCOMWAHI-S-QPCELA-OPER        PIC  9(05).
                       20  DCOMWAHI-S-REGISTROS          PIC 9(15)V9(2).
                       20  DCOMWAHI-S-PERC-REGISTROS     PIC 9(03)V9(2).
                       20  DCOMWAHI-S-TRIBUTOS           PIC 9(15)V9(2).
                       20  DCOMWAHI-S-PERC-TRIBUTOS      PIC 9(03)V9(2).
                       20  DCOMWAHI-S-TARIFAS            PIC 9(15)V9(2).
                       20  DCOMWAHI-S-PERC-TARIFAS       PIC 9(03)V9(2).
                       20  DCOMWAHI-S-PGTO-SERVICOS      PIC 9(15)V9(2).
                       20  DCOMWAHI-S-PERC-PGTO-SERVICOS PIC 9(03)V9(2).
                       20  DCOMWAHI-S-TOPER-DESC-COML    PIC  9(05).
                       20  DCOMWAHI-S-PRAZO-REMANECENTE  PIC  9(05).
                       20  DCOMWAHI-S-VSDO-DVDOR-ATU     PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VPCELA-PAGAS       PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VPCELA-ATRASO      PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VSDO-LIQUIDACAO    PIC 9(15)V9(2).
                   15  DCOMWAHI-S-QTDE-OCORR             PIC  9(03).
                   15  DCOMWAHI-S-REG-SAIDA OCCURS 0 TO 50 TIMES
                               DEPENDING ON DCOMWAHI-S-QTDE-OCORR.
                       20  DCOMWAHI-S-NPCELA-NTITLO      PIC  9(05).
                       20  DCOMWAHI-S-IPSSOA-SACDO       PIC  X(60).
                       20  DCOMWAHI-S-CPF-CNPJ-SACADO.
                         25  DCOMWAHI-S-CCNPJ-CPF-SAC    PIC  9(09).
                         25  DCOMWAHI-S-CFLIAL-CPF-SAC   PIC  9(05).
                         25  DCOMWAHI-S-CCTRL-CNPJ-CPF-SAC
                                                         PIC  9(02).
                       20  DCOMWAHI-S-DVCTO-PCELA-DESC   PIC  X(10).
                       20  DCOMWAHI-S-VPRINC-PCELA-DESC  PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VJURO-PCELA-DESC   PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VIOF-PCELA-DESC    PIC 9(15)V9(2).
                       20  DCOMWAHI-S-DEFETV-BAIXA-PCELA PIC  X(10).
                       20  DCOMWAHI-S-VEFETV-BAIXA-PCELA PIC 9(15)V9(2).
                       20  DCOMWAHI-S-VPRINC-PCELA-CALC  PIC 9(15)V9(2).
                       20  DCOMWAHI-S-TIPO-PAGADOR       PIC  X(07).
                       20  DCOMWAHI-S-IRSUMO-SIT-DESC    PIC  X(15).
