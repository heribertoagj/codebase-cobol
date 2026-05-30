      *----------------------------------------------------------------*        
      * ELEMENTO  : DBTO - DEBITO AUTOMATICO - INTERFACE BDCO          *        
      *----------------------------------------------------------------*        
      * NOME BOOK : DBTOWRE1                                           *        
      * DESCRICAO : RESOLUCAO 74 - LAYOUT 8013 TARIFAS.                *        
      * EMPRESA   : BRQ SOLUCOES                                       *        
      * AUTOR     : EVANDRO/GUILHERME BEILLO                           *        
      * TAMANHO   : 300 BYTES                                          *        
      *----------------------------------------------------------------*        
      *================================================================*        
      *                                                                         
       01 DBTOWRE1-REG.                                                         
          05 DBTOWRE1-AREA.                                                     
      *-->                                                                      
      *--> CAMPOS REFERENTE A EMPRESA(TAB DBTOB005).                            
      *-->                                                                      
             10 WRE1-CPSSOA-JURID-CONTR          PIC S9(010)     COMP-3.        
             10 WRE1-CTPO-CONTR-NEGOC            PIC S9(003)     COMP-3.        
             10 WRE1-NSEQ-CONTR-NEGOC            PIC S9(010)     COMP-3.        
             10 WRE1-CSGMTO-ATUAC-CTRTE          PIC S9(005)     COMP-3.        
             10 WRE1-IPERSO-REDZD                PIC  X(040).                   
             10 WRE1-CCPF-CNPJ-CTRTE             PIC S9(009)     COMP-3.        
             10 WRE1-CFLIAL-CNPJ-CTRTE           PIC S9(004)     COMP-3.        
             10 WRE1-CCTRL-CPF-CTRTE             PIC S9(002)     COMP-3.        
      *-->                                                                      
      *--> CAMPOS REFERENTE A EMPRESA(TAB DBTOB009).                            
      *-->                                                                      
             10 WRE1-CSERVC-CTRTE                PIC  X(020).                   
      *-->                                                                      
      *--> CAMPOS REFERENTE A EMPRESA(TAB DBTOB067).                            
      *-->                                                                      
             10 WRE1-CPSSOA-JURID-BCRIA          PIC S9(010)     COMP-3.        
             10 WRE1-CTPO-CONTR-CTRTE            PIC S9(003)     COMP-3.        
             10 WRE1-NSEQ-CONTR-CTRTE            PIC S9(010)     COMP-3.        
      *-->                                                                      
      *--> CAMPOS REFERENTE A EMPRESA(TAB DBTOB032).                            
      *-->                                                                      
             10 WRE1-CFORMA-CTRL-TARIF           PIC S9(002)     COMP-3.        
             10 WRE1-CINDCD-FORMA-TARIF          PIC S9(001)     COMP-3.        
      *-->                                                                      
      *--> CAMPOS REFERENTE A EMPRESA(TAB DBTOB033).                            
      *-->                                                                      
             10 WRE1-VTARIF-NEGOC                PIC S9(013)V99  COMP-3.        
      *-->                                                                      
      *--> CAMPOS REFERENTE A TARIFA (TAB DBTOB060).                            
      *-->                                                                      
             10 WRE1-CTPO-VNCLO-CONTR            PIC S9(003)     COMP-3.        
             10 WRE1-NSEQ-VNCLO-CTRTE            PIC S9(006)     COMP-3.        
             10 WRE1-NSEQ-IDTFD-TARIF            PIC S9(008)     COMP-3.        
             10 WRE1-CFORMA-TARIF-PROVS          PIC S9(005)     COMP-3.        
             10 WRE1-DVCTO-TARIF                 PIC  X(010).                   
             10 WRE1-DPGTO-TARIF                 PIC  X(010).                   
             10 WRE1-QDEB-EFETU-CTRTE            PIC S9(009)     COMP-3.        
             10 WRE1-QREG-APURA-REMSS            PIC S9(009)     COMP-3.        
             10 WRE1-VTARIF-PROVS-DEB            PIC S9(013)V99  COMP-3.        
      *-->                                                                      
      *--> CAMPOS FORMATADOS COM O ARQUIVO SEGTOOK.                             
      *-->                                                                      
             10 WRE1-CD-BANCO-DEBITO             PIC S9(005)     COMP-3.        
             10 WRE1-CD-AGENCIA-DEBITO           PIC S9(005)     COMP-3.        
             10 WRE1-CD-CONTA-DEBITO             PIC S9(013)     COMP-3.        
             10 WRE1-CD-DIGITO-DEBITO            PIC X(002).                    
             10 WRE1-CD-GP-CONTABIL              PIC S9(003)     COMP-3.        
             10 WRE1-CD-SUBGP-CONTABIL           PIC S9(003)     COMP-3.        
      *-->                                                                      
      *--> FILLER RESERVA                                                       
      *-->                                                                      
          05 WRE1-FILLER                         PIC X(117).                    
