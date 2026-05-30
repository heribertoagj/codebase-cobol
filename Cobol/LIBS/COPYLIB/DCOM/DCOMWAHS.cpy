      ******************************************************************
      * NOME BOOK : DCOMWAHS - BOOK DE SAIDA                           *
      * DESCRICAO : BUSCAR DADOS DO DOCUMENTO DDC                      *
      * DATA      : 21/03/2023                                         *
      * EMPRESA   : CAPGEMINI                                          *
      * ANALISTA  : LEVI BARCIELA DA SILVA CHAVES                      *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 10989 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
          05  DCOMWAHS-HEADER.
              10  DCOMWAHS-COD-LAYOUT       PIC  X(08) VALUE 'DCOMWAHS'.
              10  DCOMWAHS-TAM-LAYOUT       PIC  9(05) VALUE 10989.
          05  DCOMWAHS-REGISTRO.
               10  DCOMWAHS-BLOCO-SAIDA.
                   15  DCOMWAHS-S-REG-CONTRATO.
                       20  DCOMWAHS-S-IPSSOA-DESC-COML   PIC  X(60).
                       20  DCOMWAHS-S-ISPROD-DESC-COML   PIC  X(40).
                       20  DCOMWAHS-S-NRO-CONTRATO.
                         25  DCOMWAHS-S-DANO-OPER-DESC   PIC  9(04).
                         25  DCOMWAHS-S-NSEQ-OPER-DESC   PIC  9(09).
                       20  DCOMWAHS-S-SIT-PAGAMENTO      PIC  X(20).
                       20  DCOMWAHS-S-DVCTO-FNAL-OPER    PIC  X(10).
                       20  DCOMWAHS-S-PTX-JURO-MES       PIC 9(03)V9(5).
                       20  DCOMWAHS-S-PTX-JURO-ANO       PIC 9(03)V9(5).
                       20  DCOMWAHS-S-VCUSTO-DESC-MES    PIC 9(08)V9(7).
                       20  DCOMWAHS-S-VCUSTO-DESC-ANO    PIC 9(08)V9(7).
                       20  DCOMWAHS-S-VJURO-OPER-DESC    PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VIOF-OPER-DESC     PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VTARIF-REG-TITLO   PIC 9(15)V9(2).
                       20  DCOMWAHS-S-CPF-CNPJ.
                         25  DCOMWAHS-S-CCNPJ-CPF        PIC  9(09).
                         25  DCOMWAHS-S-CFLIAL-CPF       PIC  9(05).
                         25  DCOMWAHS-S-CCTRL-CNPJ-CPF   PIC  9(02).
                       20  DCOMWAHS-S-VOPER-DESC-COML    PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VLR-TOT-LIQ        PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VLIBRC-DESC-COML   PIC 9(15)V9(2).
                       20  DCOMWAHS-S-PERC-LIB           PIC 9(03)V9(2).
                       20  DCOMWAHS-S-DINIC-OPER-DESC    PIC  X(10).
                       20  DCOMWAHS-S-DLIBRC-DESC-COML   PIC  X(10).
                       20  DCOMWAHS-S-CTPO-DESC-COML     PIC  9(03).
                       20  DCOMWAHS-S-ITPO-DESC-COML     PIC  X(30).
                       20  DCOMWAHS-S-LIBERACAO-CALC     PIC 9(15)V9(2).
                       20  DCOMWAHS-S-SALDO-DEVEDOR      PIC 9(15)V9(2).
                       20  DCOMWAHS-S-TMED-PONDE-OPER    PIC  9(03).
                       20  DCOMWAHS-S-QPCELA-OPER        PIC  9(05).
                       20  DCOMWAHS-S-REGISTROS          PIC 9(15)V9(2).
                       20  DCOMWAHS-S-PERC-REGISTROS     PIC 9(03)V9(2).
                       20  DCOMWAHS-S-TRIBUTOS           PIC 9(15)V9(2).
                       20  DCOMWAHS-S-PERC-TRIBUTOS      PIC 9(03)V9(2).
                       20  DCOMWAHS-S-TARIFAS            PIC 9(15)V9(2).
                       20  DCOMWAHS-S-PERC-TARIFAS       PIC 9(03)V9(2).
                       20  DCOMWAHS-S-PGTO-SERVICOS      PIC 9(15)V9(2).
                       20  DCOMWAHS-S-PERC-PGTO-SERVICOS PIC 9(03)V9(2).
                       20  DCOMWAHS-S-TOPER-DESC-COML    PIC  9(05).
                       20  DCOMWAHS-S-PRAZO-REMANECENTE  PIC  9(05).
                       20  DCOMWAHS-S-VSDO-DVDOR-ATU     PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VPCELA-PAGAS       PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VPCELA-ATRASO      PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VSDO-LIQUIDACAO    PIC 9(15)V9(2).
                   15  DCOMWAHS-S-QTDE-OCORR             PIC  9(03).
                   15  DCOMWAHS-S-REG-SAIDA OCCURS 0 TO 50 TIMES
                               DEPENDING ON DCOMWAHS-S-QTDE-OCORR.
                       20  DCOMWAHS-S-NPCELA-NTITLO      PIC  9(05).
                       20  DCOMWAHS-S-IPSSOA-SACDO       PIC  X(60).
                       20  DCOMWAHS-S-CPF-CNPJ-SACADO.
                         25  DCOMWAHS-S-CCNPJ-CPF-SAC    PIC  9(09).
                         25  DCOMWAHS-S-CFLIAL-CPF-SAC   PIC  9(05).
                         25  DCOMWAHS-S-CCTRL-CNPJ-CPF-SAC
                                                         PIC  9(02).
                       20  DCOMWAHS-S-DVCTO-PCELA-DESC   PIC  X(10).
                       20  DCOMWAHS-S-VPRINC-PCELA-DESC  PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VJURO-PCELA-DESC   PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VIOF-PCELA-DESC    PIC 9(15)V9(2).
                       20  DCOMWAHS-S-DEFETV-BAIXA-PCELA PIC  X(10).
                       20  DCOMWAHS-S-VEFETV-BAIXA-PCELA PIC 9(15)V9(2).
                       20  DCOMWAHS-S-VPRINC-PCELA-CALC  PIC 9(15)V9(2).
                       20  DCOMWAHS-S-TIPO-PAGADOR       PIC  X(07).
                       20  DCOMWAHS-S-IRSUMO-SIT-DESC    PIC  X(15).
