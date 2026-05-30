      ***************************************************************           
      * NOME BOOK : DBTOWE65 - BOOK ARQUIVO TARIFA EMPRESA          *           
      *                                                             *           
      * DESCRICAO : ARQUIVO TARIFA EMPRESA (UNLOAD DBTOB060)        *           
      * DATA      : 23/07/2010                                      *           
      * AUTOR     : WAGNER SOUZA                                    *           
      * EMPRESA   : GPTI                                            *           
      * COMPONENTE: DBTO - DEBITO AUTOMATICO                        *           
      * TAMANHO   : 423   BYTES                                     *           
      ***************************************************************           
      * DATA       AUTOR             DESCRICAO / MANUTENCAO         *           
      ***************************************************************           
      *    09/2016 IEDA - FOURSYS    ALTERACAO DE TAMANHO DE CAMPOS *           
      *                              WE65-QDEB-EFETU-CTRTE          *           
      *                              WE65-QPCELA-TARIF-PROVS        *           
      *                              WE65-QREG-APURA-REMSS          *           
      *                              DE S9(5) PARA S9(9)            *           
      *                              TAMANHO: 429 BYTES             *           
      ***************************************************************           
       01  WE65-REGISTRO.                                                       
           10  WE65-CPSSOA-JURID-CONTR         PIC S9(10)       COMP-3.         
           10  WE65-CTPO-CONTR-NEGOC           PIC S9(3)        COMP-3.         
           10  WE65-NSEQ-CONTR-NEGOC           PIC S9(10)       COMP-3.         
           10  WE65-CTPO-VNCLO-CONTR           PIC S9(3)        COMP-3.         
           10  WE65-NSEQ-VNCLO-CTRTE           PIC S9(6)        COMP-3.         
           10  WE65-NSEQ-IDTFD-TARIF           PIC S9(8)        COMP-3.         
           10  WE65-CTPO-CTRL-DEB              PIC S9(5)        COMP-3.         
           10  WE65-CTPO-CTRL-DEB-NULL         PIC  X(01).                      
           10  WE65-CTPO-PGTO-TARIF            PIC S9(5)        COMP-3.         
           10  WE65-CTPO-PGTO-TARIF-NULL       PIC  X(01).                      
           10  WE65-CFORMA-TARIF-PROVS         PIC S9(2)        COMP-3.         
           10  WE65-CFORMA-TARIF-PROVS-NULL    PIC  X(01).                      
           10  WE65-DVCTO-TARIF                PIC  X(10).                      
           10  WE65-DVCTO-TARIF-NULL           PIC  X(01).                      
           10  WE65-DPGTO-TARIF                PIC  X(10).                      
           10  WE65-DPGTO-TARIF-NULL           PIC  X(01).                      
           10  WE65-DINIC-APURC-TARIF          PIC  X(10).                      
           10  WE65-DINIC-APURC-TARIF-NULL     PIC  X(01).                      
           10  WE65-DFIM-APURC-TARIF           PIC  X(10).                      
           10  WE65-DFIM-APURC-TARIF-NULL      PIC  X(01).                      
           10  WE65-HBAIXA-TARIF               PIC  X(26).                      
           10  WE65-HBAIXA-TARIF-NULL          PIC  X(01).                      
           10  WE65-HCANCT-TARIF               PIC  X(26).                      
           10  WE65-HCANCT-TARIF-NULL          PIC  X(01).                      
           10  WE65-HENVIO-CARTA-TARIF         PIC  X(26).                      
           10  WE65-HENVIO-CARTA-TARIF-NULL    PIC  X(01).                      
           10  WE65-QDEB-EFETU-CTRTE           PIC S9(9)        COMP-3.         
           10  WE65-QDEB-EFETU-CTRTE-NULL      PIC  X(01).                      
           10  WE65-QPCELA-TARIF-PROVS         PIC S9(9)        COMP-3.         
           10  WE65-QPCELA-TARIF-PROVS-NULL    PIC  X(01).                      
           10  WE65-QREG-APURA-REMSS           PIC S9(9)        COMP-3.         
           10  WE65-QREG-APURA-REMSS-NULL      PIC  X(01).                      
           10  WE65-VENCAR-TARIF-PROVS         PIC S9(13)V9(2)  COMP-3.         
           10  WE65-VENCAR-TARIF-PROVS-NULL    PIC  X(01).                      
           10  WE65-VPGTO-EFTVC                PIC S9(13)V9(2)  COMP-3.         
           10  WE65-VPGTO-EFTVC-NULL           PIC  X(01).                      
           10  WE65-VTOT-DEB-DIA               PIC S9(15)V9(2)  COMP-3.         
           10  WE65-VTOT-DEB-DIA-NULL          PIC  X(01).                      
           10  WE65-VTARIF-PROVS-DEB           PIC S9(13)V9(2)  COMP-3.         
           10  WE65-VTARIF-PROVS-DEB-NULL      PIC  X(01).                      
           10  WE65-VAJUST-TARIF-DEB           PIC S9(13)V9(2)  COMP-3.         
           10  WE65-VAJUST-TARIF-DEB-NULL      PIC  X(01).                      
           10  WE65-CINDCD-ENVIO-AUTOM         PIC  X(1).                       
           10  WE65-CDOCTO-LCTO-TARIF          PIC  X(15).                      
           10  WE65-CDOCTO-LCTO-TARIF-NULL     PIC  X(01).                      
           10  WE65-CPSSOA-NEGOC-OPTE          PIC S9(10)       COMP-3.         
           10  WE65-CPSSOA-NEGOC-OPTE-NULL     PIC  X(01).                      
           10  WE65-CTPO-NEGOC-OPTE            PIC S9(3)        COMP-3.         
           10  WE65-CTPO-NEGOC-OPTE-NULL       PIC  X(01).                      
           10  WE65-NSEQ-NEGOC-OPTE            PIC S9(10)       COMP-3.         
           10  WE65-NSEQ-NEGOC-OPTE-NULL       PIC  X(01).                      
           10  WE65-CPSSOA-JURID-CTRTE         PIC S9(10)       COMP-3.         
           10  WE65-CPSSOA-JURID-CTRTE-NULL    PIC  X(01).                      
           10  WE65-CTPO-CONTR-CTRTE           PIC S9(3)        COMP-3.         
           10  WE65-CTPO-CONTR-CTRTE-NULL      PIC  X(01).                      
           10  WE65-NSEQ-CONTR-CTRTE           PIC S9(10)       COMP-3.         
           10  WE65-NSEQ-CONTR-CTRTE-NULL      PIC  X(01).                      
           10  WE65-NSEQ-MOVTO-CTRTE           PIC S9(8)        COMP-3.         
           10  WE65-NSEQ-MOVTO-CTRTE-NULL      PIC  X(01).                      
           10  WE65-CUSUAR-MANUT               PIC  X(9).                       
           10  WE65-CUSUAR-MANUT-NULL          PIC  X(01).                      
           10  WE65-HMANUT-REG                 PIC  X(26).                      
           10  WE65-CPSSOA-JURID               PIC S9(10)       COMP-3.         
           10  WE65-CPSSOA-JURID-NULL          PIC  X(01).                      
           10  WE65-NSEQ-UND-ORGNZ             PIC S9(8)        COMP-3.         
           10  WE65-NSEQ-UND-ORGNZ-NULL        PIC  X(01).                      
           10  WE65-CPSSOA-LOTE-AUTOM          PIC S9(10)       COMP-3.         
           10  WE65-CPSSOA-LOTE-AUTOM-NULL     PIC  X(01).                      
           10  WE65-CSIST                      PIC  X(4).                       
           10  WE65-CSIST-NULL                 PIC  X(01).                      
           10  WE65-CLOTE                      PIC  X(9).                       
           10  WE65-CLOTE-NULL                 PIC  X(01).                      
           10  WE65-NLOTE                      PIC S9(18)       COMP-3.         
           10  WE65-NLOTE-NULL                 PIC  X(01).                      
WW         10  WE65-CCONC-EVNTO-CTBIL          PIC  X(25).                      
WW         10  WE65-CCONC-EVNTO-CTBIL-NULL     PIC  X(1).                       
WW         10  WE65-CBCO-TARIF-ESPEC           PIC S9(3)        COMP-3.         
WW         10  WE65-CBCO-TARIF-ESPEC-NULL      PIC  X(1).                       
WW         10  WE65-CAG-TARIF-ESPEC            PIC S9(5)        COMP-3.         
WW         10  WE65-CAG-TARIF-ESPEC-NULL       PIC  X(1).                       
WW         10  WE65-CCTA-TARIF-ESPEC           PIC S9(13)       COMP-3.         
WW         10  WE65-CCTA-TARIF-ESPEC-NULL      PIC  X(1).                       
WW         10  WE65-CDIG-CTA-TARIF             PIC  X(2).                       
WW         10  WE65-CDIG-CTA-TARIF-NULL        PIC  X(1).                       
WW         10  WE65-CTPO-CTA-BCRIA             PIC S9(5)        COMP-3.         
WW         10  WE65-CTPO-CTA-BCRIA-NULL        PIC  X(01).                      
WW         10  WE65-CCPF-TARIF-ESPEC           PIC S9(9)        COMP-3.         
WW         10  WE65-CCPF-TARIF-ESPEC-NULL      PIC  X(01).                      
WW         10  WE65-CFLIAL-CNPJ-TARIF          PIC S9(4)        COMP-3.         
WW         10  WE65-CFLIAL-CNPJ-TARIF-NULL     PIC  X(01).                      
WW         10  WE65-CCTRL-CPF-TARIF            PIC S9(2)        COMP-3.         
WW         10  WE65-CCTRL-CPF-TARIF-NULL       PIC  X(01).                      
WW         10  WE65-CTPO-FORMA-ALTRN           PIC S9(5)        COMP-3.         
WW         10  WE65-CTPO-FORMA-ALTRN-NULL      PIC  X(01).                      
